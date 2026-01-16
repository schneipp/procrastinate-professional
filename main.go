package main

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"time"

	"github.com/gomarkdown/markdown"
	"github.com/gomarkdown/markdown/html"
	"github.com/gomarkdown/markdown/parser"
	"github.com/gorilla/websocket"
	"github.com/joho/godotenv"
	"github.com/openai/openai-go"
	"github.com/openai/openai-go/option"
	"github.com/openai/openai-go/shared"
	"mvdan.cc/xurls/v2"
)

type OpenAIResponse struct {
	Category string   `json:"category"`
	Tags     []string `json:"tags"`
	Duedate  string   `json:"duedate"`
	Summary  string   `json:"summary,omitempty"`
}

// Todo represents a single todo item
type Todo struct {
	ID         int64    `json:"id"`
	Text       string   `json:"text"`
	Category   string   `json:"category"`
	Tags       []string `json:"tags"`
	Color      string   `json:"color"`
	Duedate    string   `json:"duedate"`
	Completed  bool     `json:"completed"`
	Owner      string   `json:"owner"`       // Username of the owner
	SharedWith []string `json:"shared_with"` // List of usernames this todo is shared with
}

// Message represents a WebSocket message
type Message struct {
	Type     string   `json:"type"`
	Todo     *Todo    `json:"todo,omitempty"`
	TodoID   int64    `json:"todoId,omitempty"`
	Todos    []Todo   `json:"todos,omitempty"`
	Users    []string `json:"users,omitempty"`
	Username string   `json:"username,omitempty"`
}

// Client represents a connected WebSocket client
type Client struct {
	Conn     *websocket.Conn
	Username string
}

var (
	upgrader = websocket.Upgrader{
		CheckOrigin: func(r *http.Request) bool {
			return true
		},
	}
	clients   = make(map[*websocket.Conn]*Client)
	broadcast = make(chan BroadcastMessage)
	mutex     = &sync.Mutex{}
	todos     = make([]Todo, 0)
	todosDir  = "./todos"
)

// BroadcastMessage contains message data and target users
type BroadcastMessage struct {
	Data        []byte
	TargetUsers []string // Empty means broadcast to all
}

// loadTodos loads all todos from JSON files in the todos directory
func loadTodos() error {
	// Create todos directory if it doesn't exist
	if err := os.MkdirAll(todosDir, 0755); err != nil {
		return fmt.Errorf("failed to create todos directory: %v", err)
	}

	// Read all files in the todos directory
	files, err := os.ReadDir(todosDir)
	if err != nil {
		return fmt.Errorf("failed to read todos directory: %v", err)
	}

	for _, file := range files {
		if filepath.Ext(file.Name()) != ".json" {
			continue
		}

		data, err := os.ReadFile(filepath.Join(todosDir, file.Name()))
		if err != nil {
			fmt.Printf("Error reading todo file %s: %v\n", file.Name(), err)
			continue
		}

		var todo Todo
		if err := json.Unmarshal(data, &todo); err != nil {
			fmt.Printf("Error parsing todo file %s: %v\n", file.Name(), err)
			continue
		}

		fmt.Printf("Loaded todo from %s: ID=%d, Text=%s, Category=%s, Tags=%v, Duedate=%s, Owner=%s\n",
			file.Name(), todo.ID, todo.Text, todo.Category, todo.Tags, todo.Duedate, todo.Owner)

		format_urls(&todo)

		todos = append(todos, todo)
	}

	return nil
}

func mdToHTML(md []byte) []byte {
	// create markdown parser with extensions
	extensions := parser.CommonExtensions | parser.AutoHeadingIDs | parser.NoEmptyLineBeforeBlock
	p := parser.NewWithExtensions(extensions)
	doc := p.Parse(md)

	// create HTML renderer with extensions
	htmlFlags := html.CommonFlags | html.HrefTargetBlank
	opts := html.RendererOptions{Flags: htmlFlags}
	renderer := html.NewRenderer(opts)

	return markdown.Render(doc, renderer)
}

func format_urls(todo *Todo) {
	re := regexp.MustCompile(`https?://[^\s]+`)
	found := re.FindString(todo.Text)
	if found == "" {
		fmt.Println("No URL found")
		return
	}

	parsedURL, err := url.Parse(found)
	if err != nil {
		fmt.Println("Error parsing URL:", err)
		return
	}

	pathSegments := strings.Split(strings.Trim(parsedURL.Path, "/"), "/")
	basename := ""
	if len(pathSegments) > 0 {
		basename = pathSegments[len(pathSegments)-1]
	} else {
		basename = parsedURL.Host
	}

	htmlLink := fmt.Sprintf(`<a target="_blank" href="%s">%s</a>`, found, basename)
	todo.Text = re.ReplaceAllStringFunc(todo.Text, func(s string) string {
		if s == found {
			found = ""
			return htmlLink
		}
		return s
	})

	fmt.Println(htmlLink)

	todo.Text = strings.ReplaceAll(todo.Text, "```html", "")
	todo.Text = strings.ReplaceAll(todo.Text, "```", "")
}

func openai_tag_and_analyze(todo *Todo) {
	openai_key := os.Getenv("OPENAI_API_KEY")
	if openai_key == "" {
		println("OPENAI_API_KEY not set in .env file")
	}
	print(openai_key)
	client := openai.NewClient(
		option.WithAPIKey(openai_key),
	)
	date_today := time.Now().Format("2006-01-02")
	prompt := "You are a category classifier for todo items."
	prompt += "Given the todo text, classify it into one of the following categories: "
	prompt += "todo, task. it is a task if there someting with time associated with it, otherwise it is a todo. add a list of tags that fit the provided input.  always repsond in json. no markup. JSON answer only. today is the following date: " + date_today + ".\n"
	prompt += "Format the Response as JSON with the fields 'category' and 'tags' and 'duedate'. Add Tags and categories in the language of the input text \n\n\nThe input text to analyze is: " + todo.Text

	format := shared.NewResponseFormatJSONObjectParam()
	chatCompletion, err := client.Chat.Completions.New(context.TODO(), openai.ChatCompletionNewParams{
		Messages: []openai.ChatCompletionMessageParamUnion{
			openai.UserMessage(prompt),
		},
		ResponseFormat: openai.ChatCompletionNewParamsResponseFormatUnion{
			OfJSONObject: &format,
		},
		Model: openai.ChatModelGPT4o,
	})
	if err != nil {
		panic(err.Error())
	}
	var response OpenAIResponse
	cleanup2 := chatCompletion.Choices[0].Message.Content
	err = json.Unmarshal([]byte(cleanup2), &response)
	if err != nil {
		println("Error unmarshalling response:", err.Error())
	}
	todo.Tags = response.Tags
	todo.Category = response.Category
	todo.Duedate = response.Duedate
}

func openai_summarizeurls(todo *Todo) {
	openai_key := os.Getenv("OPENAI_API_KEY")
	if openai_key == "" {
		println("OPENAI_API_KEY not set in .env file")
	}
	client := openai.NewClient(
		option.WithAPIKey(openai_key),
	)
	if strings.Contains(todo.Text, "https://") == false && strings.Contains(todo.Text, "http://") == false {
		return
	}

	extractor := xurls.Relaxed()
	url := extractor.FindString(todo.Text)
	prompt := "You provide a summary in html format. you only respond in html for the provided url. only a few sentences. do NOT use markdown for link formatting. use html link with target=blank. if there is contact information provided, add that" + url
	chatCompletion, err := client.Chat.Completions.New(context.TODO(), openai.ChatCompletionNewParams{
		Messages: []openai.ChatCompletionMessageParamUnion{
			openai.UserMessage(prompt),
		},
		Model: openai.ChatModelGPT4oSearchPreview,
	})
	if err != nil {
		panic(err.Error())
	}
	summary := chatCompletion.Choices[0].Message.Content
	todo.Text += "    <br /><br />" + summary
}

// saveTodo saves a single todo to a JSON file
func saveTodo(todo Todo) error {
	data, err := json.Marshal(todo)
	if err != nil {
		return fmt.Errorf("failed to marshal todo: %v", err)
	}

	filename := filepath.Join(todosDir, fmt.Sprintf("%d.json", todo.ID))
	return os.WriteFile(filename, data, 0644)
}

// deleteTodo removes a todo file
func deleteTodo(id int64) error {
	filename := filepath.Join(todosDir, fmt.Sprintf("%d.json", id))
	return os.Remove(filename)
}

// canAccessTodo checks if a user can access a todo
func canAccessTodo(todo *Todo, username string) bool {
	if todo.Owner == "" || todo.Owner == username {
		return true
	}
	for _, sharedUser := range todo.SharedWith {
		if sharedUser == username {
			return true
		}
	}
	return false
}

// getTodosForUser returns todos that belong to or are shared with a user
func getTodosForUser(username string) []Todo {
	mutex.Lock()
	defer mutex.Unlock()

	userTodos := make([]Todo, 0)
	for _, todo := range todos {
		if canAccessTodo(&todo, username) {
			userTodos = append(userTodos, todo)
		}
	}
	return userTodos
}

// getUsersForTodo returns list of users who should receive updates for a todo
func getUsersForTodo(todo *Todo) []string {
	users := []string{todo.Owner}
	for _, sharedUser := range todo.SharedWith {
		users = append(users, sharedUser)
	}
	return users
}

func wsHandler(w http.ResponseWriter, r *http.Request) {
	// Check authentication
	username := GetUsernameFromRequest(r)
	if username == "" {
		http.Error(w, "Unauthorized", http.StatusUnauthorized)
		return
	}

	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println("Error upgrading:", err)
		return
	}
	defer conn.Close()

	client := &Client{
		Conn:     conn,
		Username: username,
	}

	mutex.Lock()
	clients[conn] = client
	mutex.Unlock()

	// Send initial todo list to the new client (only their todos)
	userTodos := getTodosForUser(username)
	msg := Message{
		Type:     "todo_list",
		Todos:    userTodos,
		Users:    GetAllUsers(),
		Username: username,
	}
	data, _ := json.Marshal(msg)
	fmt.Printf("Sending initial todo list to %s: %d todos\n", username, len(userTodos))
	conn.WriteMessage(websocket.TextMessage, data)

	for {
		_, message, err := conn.ReadMessage()
		if err != nil {
			mutex.Lock()
			delete(clients, conn)
			mutex.Unlock()
			break
		}

		var msg Message
		if err := json.Unmarshal(message, &msg); err != nil {
			fmt.Printf("Error parsing message: %v\n", err)
			continue
		}

		switch msg.Type {
		case "add_todo":
			if msg.Todo != nil {
				// Set owner to current user
				msg.Todo.Owner = username
				if msg.Todo.SharedWith == nil {
					msg.Todo.SharedWith = []string{}
				}

				// Analyze the todo first
				openai_summarizeurls(msg.Todo)
				openai_tag_and_analyze(msg.Todo)

				mutex.Lock()
				todos = append(todos, *msg.Todo)
				mutex.Unlock()

				if err := saveTodo(*msg.Todo); err != nil {
					fmt.Printf("Error saving todo: %v\n", err)
				}
				format_urls(msg.Todo)

				msg.Type = "todo_added"
				data, _ := json.Marshal(msg)
				fmt.Printf("Sending todo_added message: %s\n", string(data))

				// Broadcast to owner and shared users
				broadcast <- BroadcastMessage{
					Data:        data,
					TargetUsers: getUsersForTodo(msg.Todo),
				}
				continue
			}

		case "update_todo":
			if msg.Todo != nil {
				mutex.Lock()
				for i, todo := range todos {
					if todo.ID == msg.Todo.ID {
						// Only owner can update sharing settings
						if todo.Owner != username && todo.Owner != "" {
							msg.Todo.SharedWith = todo.SharedWith
							msg.Todo.Owner = todo.Owner
						}
						todos[i] = *msg.Todo
						if err := saveTodo(*msg.Todo); err != nil {
							fmt.Printf("Error updating todo: %v\n", err)
						}
						break
					}
				}
				mutex.Unlock()

				msg.Type = "todo_updated"
				data, _ := json.Marshal(msg)

				// Broadcast to owner and shared users
				broadcast <- BroadcastMessage{
					Data:        data,
					TargetUsers: getUsersForTodo(msg.Todo),
				}
				continue
			}

		case "delete_todo":
			if msg.TodoID != 0 {
				var targetUsers []string
				mutex.Lock()
				for i, todo := range todos {
					if todo.ID == msg.TodoID {
						// Only owner can delete
						if todo.Owner != username && todo.Owner != "" {
							mutex.Unlock()
							continue
						}
						targetUsers = getUsersForTodo(&todo)
						todos = append(todos[:i], todos[i+1:]...)
						if err := deleteTodo(msg.TodoID); err != nil {
							fmt.Printf("Error deleting todo: %v\n", err)
						}
						break
					}
				}
				mutex.Unlock()

				msg.Type = "todo_deleted"
				data, _ := json.Marshal(msg)

				broadcast <- BroadcastMessage{
					Data:        data,
					TargetUsers: targetUsers,
				}
				continue
			}

		case "share_todo":
			if msg.Todo != nil {
				mutex.Lock()
				for i, todo := range todos {
					if todo.ID == msg.Todo.ID {
						// Only owner can share
						if todo.Owner != username {
							mutex.Unlock()
							continue
						}
						todos[i].SharedWith = msg.Todo.SharedWith
						if err := saveTodo(todos[i]); err != nil {
							fmt.Printf("Error saving shared todo: %v\n", err)
						}

						msg.Type = "todo_updated"
						msg.Todo = &todos[i]
						data, _ := json.Marshal(msg)

						// Notify all affected users
						broadcast <- BroadcastMessage{
							Data:        data,
							TargetUsers: getUsersForTodo(&todos[i]),
						}
						break
					}
				}
				mutex.Unlock()
				continue
			}
		}

		data, _ := json.Marshal(msg)
		broadcast <- BroadcastMessage{
			Data:        data,
			TargetUsers: []string{username},
		}
	}
}

func handleMessages() {
	for {
		message := <-broadcast
		mutex.Lock()
		for conn, client := range clients {
			// Check if this client should receive the message
			shouldSend := len(message.TargetUsers) == 0
			if !shouldSend {
				for _, targetUser := range message.TargetUsers {
					if client.Username == targetUser {
						shouldSend = true
						break
					}
				}
			}

			if shouldSend {
				err := conn.WriteMessage(websocket.TextMessage, message.Data)
				if err != nil {
					conn.Close()
					delete(clients, conn)
				}
			}
		}
		mutex.Unlock()
	}
}

// API Handlers

func loginHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req struct {
		Username string `json:"username"`
		Password string `json:"password"`
	}

	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error": "Invalid request"}`, http.StatusBadRequest)
		return
	}

	if !ValidateCredentials(req.Username, req.Password) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusUnauthorized)
		json.NewEncoder(w).Encode(map[string]string{"error": "Invalid username or password"})
		return
	}

	token, err := CreateSession(req.Username)
	if err != nil {
		http.Error(w, `{"error": "Failed to create session"}`, http.StatusInternalServerError)
		return
	}

	// Set session cookie (2 weeks expiry)
	http.SetCookie(w, &http.Cookie{
		Name:     "session_token",
		Value:    token,
		Path:     "/",
		MaxAge:   14 * 24 * 60 * 60, // 2 weeks in seconds
		HttpOnly: true,
		SameSite: http.SameSiteLaxMode,
	})

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{"status": "ok", "username": req.Username})
}

func logoutHandler(w http.ResponseWriter, r *http.Request) {
	cookie, err := r.Cookie("session_token")
	if err == nil {
		InvalidateSession(cookie.Value)
	}

	http.SetCookie(w, &http.Cookie{
		Name:     "session_token",
		Value:    "",
		Path:     "/",
		MaxAge:   -1,
		HttpOnly: true,
	})

	http.Redirect(w, r, "/login", http.StatusSeeOther)
}

func usersHandler(w http.ResponseWriter, r *http.Request) {
	username := GetUsernameFromRequest(r)
	if username == "" {
		http.Error(w, `{"error": "Unauthorized"}`, http.StatusUnauthorized)
		return
	}

	switch r.Method {
	case http.MethodGet:
		// Get all users (for sharing dropdown)
		users := GetAllUsers()
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(users)

	case http.MethodPost:
		// Create new user (admin only)
		if !IsAdmin(username) {
			http.Error(w, `{"error": "Admin access required"}`, http.StatusForbidden)
			return
		}

		var req struct {
			Username string `json:"username"`
			Password string `json:"password"`
			IsAdmin  bool   `json:"is_admin"`
		}

		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			http.Error(w, `{"error": "Invalid request"}`, http.StatusBadRequest)
			return
		}

		if req.Username == "" || req.Password == "" {
			http.Error(w, `{"error": "Username and password required"}`, http.StatusBadRequest)
			return
		}

		if err := CreateUser(req.Username, req.Password, req.IsAdmin); err != nil {
			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusBadRequest)
			json.NewEncoder(w).Encode(map[string]string{"error": err.Error()})
			return
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{"status": "ok"})

	case http.MethodDelete:
		// Delete user (admin only)
		if !IsAdmin(username) {
			http.Error(w, `{"error": "Admin access required"}`, http.StatusForbidden)
			return
		}

		targetUser := r.URL.Query().Get("username")
		if targetUser == "" {
			http.Error(w, `{"error": "Username required"}`, http.StatusBadRequest)
			return
		}

		if targetUser == username {
			http.Error(w, `{"error": "Cannot delete yourself"}`, http.StatusBadRequest)
			return
		}

		if err := DeleteUser(targetUser); err != nil {
			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusBadRequest)
			json.NewEncoder(w).Encode(map[string]string{"error": err.Error()})
			return
		}

		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{"status": "ok"})

	default:
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
	}
}

func changePasswordHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	username := GetUsernameFromRequest(r)
	if username == "" {
		http.Error(w, `{"error": "Unauthorized"}`, http.StatusUnauthorized)
		return
	}

	var req struct {
		CurrentPassword string `json:"current_password"`
		NewPassword     string `json:"new_password"`
	}

	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error": "Invalid request"}`, http.StatusBadRequest)
		return
	}

	// Verify current password
	if !ValidateCredentials(username, req.CurrentPassword) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusUnauthorized)
		json.NewEncoder(w).Encode(map[string]string{"error": "Current password is incorrect"})
		return
	}

	if err := ChangePassword(username, req.NewPassword); err != nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusInternalServerError)
		json.NewEncoder(w).Encode(map[string]string{"error": err.Error()})
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{"status": "ok"})
}

func meHandler(w http.ResponseWriter, r *http.Request) {
	username := GetUsernameFromRequest(r)
	if username == "" {
		http.Error(w, `{"error": "Unauthorized"}`, http.StatusUnauthorized)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"username": username,
		"is_admin": IsAdmin(username),
	})
}

// Device Auth Handlers

func deviceAuthInitHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Allow CORS for CLI clients
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

	if r.Method == http.MethodOptions {
		return
	}

	req := CreateDeviceAuthRequest()

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"device_code":      req.DeviceCode,
		"user_code":        req.UserCode,
		"expires_in":       600, // 10 minutes
		"interval":         5,   // poll every 5 seconds
		"verification_uri": "/auth/device",
	})
}

func deviceAuthPollHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Allow CORS for CLI clients
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

	var req struct {
		DeviceCode string `json:"device_code"`
	}

	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error": "invalid_request"}`, http.StatusBadRequest)
		return
	}

	authReq := GetDeviceAuthByDeviceCode(req.DeviceCode)
	if authReq == nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]string{"error": "expired_token"})
		return
	}

	w.Header().Set("Content-Type", "application/json")

	if !authReq.Approved {
		w.WriteHeader(http.StatusAccepted)
		json.NewEncoder(w).Encode(map[string]string{"error": "authorization_pending"})
		return
	}

	// Auth approved - return the session token
	response := map[string]string{
		"access_token": authReq.SessionToken,
		"token_type":   "session",
		"username":     authReq.Username,
	}

	// Clean up the device auth request
	RemoveDeviceAuth(req.DeviceCode)

	json.NewEncoder(w).Encode(response)
}

func deviceAuthPageHandler(w http.ResponseWriter, r *http.Request) {
	// Check if user is logged in
	username := GetUsernameFromRequest(r)
	if username == "" {
		// Redirect to login with return URL
		http.Redirect(w, r, "/login?redirect=/auth/device", http.StatusSeeOther)
		return
	}

	http.ServeFile(w, r, "./static/device-auth.html")
}

func deviceAuthApproveHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	username := GetUsernameFromRequest(r)
	if username == "" {
		http.Error(w, `{"error": "Unauthorized"}`, http.StatusUnauthorized)
		return
	}

	var req struct {
		UserCode string `json:"user_code"`
	}

	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, `{"error": "Invalid request"}`, http.StatusBadRequest)
		return
	}

	// Normalize user code (remove dashes, uppercase)
	userCode := strings.ToUpper(strings.ReplaceAll(req.UserCode, "-", ""))
	if len(userCode) == 8 {
		userCode = userCode[:4] + "-" + userCode[4:]
	}

	if err := ApproveDeviceAuth(userCode, username); err != nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusBadRequest)
		json.NewEncoder(w).Encode(map[string]string{"error": err.Error()})
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{"status": "ok"})
}

func main() {
	godotenv.Load()

	// Initialize authentication
	if err := InitAuth(); err != nil {
		fmt.Printf("Error initializing auth: %v\n", err)
		os.Exit(1)
	}

	// Load existing todos
	if err := loadTodos(); err != nil {
		fmt.Printf("Error loading todos: %v\n", err)
	}

	// Serve static files (protected by auth check in index.html redirect)
	http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(http.Dir("./static"))))

	// Login page (public)
	http.HandleFunc("/login", func(w http.ResponseWriter, r *http.Request) {
		// If already logged in, redirect to app
		if username := GetUsernameFromRequest(r); username != "" {
			http.Redirect(w, r, "/static/index.html", http.StatusSeeOther)
			return
		}
		http.ServeFile(w, r, "./static/login.html")
	})

	// Root redirect
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/" {
			http.NotFound(w, r)
			return
		}
		// Check if logged in
		if username := GetUsernameFromRequest(r); username != "" {
			http.Redirect(w, r, "/static/index.html", http.StatusSeeOther)
		} else {
			http.Redirect(w, r, "/login", http.StatusSeeOther)
		}
	})

	// API endpoints
	http.HandleFunc("/api/login", loginHandler)
	http.HandleFunc("/api/logout", logoutHandler)
	http.HandleFunc("/api/users", usersHandler)
	http.HandleFunc("/api/me", meHandler)
	http.HandleFunc("/api/change-password", changePasswordHandler)

	// Device auth endpoints (for CLI/editor clients)
	http.HandleFunc("/api/device/code", deviceAuthInitHandler)
	http.HandleFunc("/api/device/token", deviceAuthPollHandler)
	http.HandleFunc("/auth/device", deviceAuthPageHandler)
	http.HandleFunc("/api/device/approve", deviceAuthApproveHandler)

	// WebSocket endpoint (protected)
	http.HandleFunc("/ws", wsHandler)

	// Start message handler
	go handleMessages()

	fmt.Println("WebSocket server started on :8090")
	if err := http.ListenAndServe(":8090", nil); err != nil {
		fmt.Println("Error starting server:", err)
	}
}
