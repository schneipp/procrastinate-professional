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
	ID        int64    `json:"id"`
	Text      string   `json:"text"`
	Category  string   `json:"category"`
	Tags      []string `json:"tags"`
	Color     string   `json:"color"`
	Duedate   string   `json:"duedate"`
	Completed bool     `json:"completed"`
}

// Message represents a WebSocket message
type Message struct {
	Type   string `json:"type"`
	Todo   *Todo  `json:"todo,omitempty"`
	TodoID int64  `json:"todoId,omitempty"`
	Todos  []Todo `json:"todos,omitempty"`
}

var (
	upgrader = websocket.Upgrader{
		CheckOrigin: func(r *http.Request) bool {
			return true
		},
	}
	clients   = make(map[*websocket.Conn]bool)
	broadcast = make(chan []byte)
	mutex     = &sync.Mutex{}
	todos     = make([]Todo, 0)
	todosDir  = "./todos"
)

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

		fmt.Printf("Loaded todo from %s: ID=%d, Text=%s, Category=%s, Tags=%v, Duedate=%s\n",
			file.Name(), todo.ID, todo.Text, todo.Category, todo.Tags, todo.Duedate)

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
	// convert markdown to html
	// markdown := []byte(todo.Text)
	// todo.Text = string(mdToHTML(markdown))
	// extract the first url and shorten it
	// https://asdfasdf.com/fdsafd/%fdsfd => should be asdfasdf.com
	re := regexp.MustCompile(`https?://[^\s]+`)
	found := re.FindString(todo.Text)
	if found == "" {
		fmt.Println("No URL found")
		return
	}

	// Parse the URL to get the path basename
	parsedURL, err := url.Parse(found)
	if err != nil {
		fmt.Println("Error parsing URL:", err)
		return
	}

	// Get the last segment of the path
	pathSegments := strings.Split(strings.Trim(parsedURL.Path, "/"), "/")
	basename := ""
	if len(pathSegments) > 0 {
		basename = pathSegments[len(pathSegments)-1]
	} else {
		basename = parsedURL.Host // fallback to host if path empty
	}

	// Create the HTML anchor tag
	htmlLink := fmt.Sprintf(`<a target="_blank" href="%s">%s</a>`, found, basename)
	// todo.Text = re.ReplaceAllString(todo.Text, htmlLink)
	todo.Text = re.ReplaceAllStringFunc(todo.Text, func(s string) string {
		if s == found {
			// Replace once, then subsequent calls return original string
			found = "" // clear so we don't replace again
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
		option.WithAPIKey(openai_key), // defaults to os.LookupEnv("OPENAI_API_KEY")
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
		// Model: openai.ChatModelGPT4oSearchPreview,
	})
	if err != nil {
		panic(err.Error())
	}
	var response OpenAIResponse
	cleanup2 := chatCompletion.Choices[0].Message.Content
	// remove the first and alast line of the response, which is not valid JSON
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
		option.WithAPIKey(openai_key), // defaults to os.LookupEnv("OPENAI_API_KEY")
	)
	if strings.Contains(todo.Text, "https://") == false && strings.Contains(todo.Text, "http://") == false {
		return // no url to summarize
	}

	extractor := xurls.Relaxed()
	url := extractor.FindString(todo.Text)
	// for _, url := range urls {
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

//}

// saveTodo saves a single todo to a JSON file
func saveTodo(todo Todo) error {
	// First analyze the todo with OpenAI
	//
	//
	// openai_tag_and_analyze(&todo)
	// openai_summarizeurls(&todo)

	// Then marshal and save
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

func wsHandler(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println("Error upgrading:", err)
		return
	}
	defer conn.Close()

	mutex.Lock()
	clients[conn] = true
	mutex.Unlock()

	// Send initial todo list to the new client
	msg := Message{
		Type:  "todo_list",
		Todos: todos,
	}
	data, _ := json.Marshal(msg)
	fmt.Printf("Sending initial todo list: %s\n", string(data))
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
				// Analyze the todo first
				openai_summarizeurls(msg.Todo)
				openai_tag_and_analyze(msg.Todo)
				// Then add it to the list
				todos = append(todos, *msg.Todo)
				if err := saveTodo(*msg.Todo); err != nil {
					fmt.Printf("Error saving todo: %v\n", err)
				}
				format_urls(msg.Todo)
				// reset
				msg.Type = "todo_added"
				data, _ := json.Marshal(msg)
				fmt.Printf("Sending todo_added message: %s\n", string(data))
			}

		case "update_todo":
			if msg.Todo != nil {
				for i, todo := range todos {
					if todo.ID == msg.Todo.ID {
						todos[i] = *msg.Todo
						if err := saveTodo(*msg.Todo); err != nil {
							fmt.Printf("Error updating todo: %v\n", err)
						}
						break
					}
				}
				msg.Type = "todo_updated"
			}

		case "delete_todo":
			if msg.TodoID != 0 {
				for i, todo := range todos {
					if todo.ID == msg.TodoID {
						todos = append(todos[:i], todos[i+1:]...)
						if err := deleteTodo(msg.TodoID); err != nil {
							fmt.Printf("Error deleting todo: %v\n", err)
						}
						break
					}
				}
				msg.Type = "todo_deleted"
			}
		}

		data, _ := json.Marshal(msg)
		broadcast <- data
	}
}

func handleMessages() {
	for {
		message := <-broadcast
		mutex.Lock()
		for client := range clients {
			err := client.WriteMessage(websocket.TextMessage, message)
			if err != nil {
				client.Close()
				delete(clients, client)
			}
		}
		mutex.Unlock()
	}
}

func main() {
	// Load existing todos
	if err := loadTodos(); err != nil {
		fmt.Printf("Error loading todos: %v\n", err)
	}
	godotenv.Load()

	// Serve static files
	http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(http.Dir("./static"))))
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/html")
		w.Write([]byte(`<script>window.location.href = "/static/index.html";</script>`))
	})

	// WebSocket endpoint
	http.HandleFunc("/ws", wsHandler)

	// Start message handler
	go handleMessages()

	fmt.Println("WebSocket server started on :8090")
	if err := http.ListenAndServe(":8090", nil); err != nil {
		fmt.Println("Error starting server:", err)
	}
}
