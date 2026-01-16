package main

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"sync"
	"time"

	"golang.org/x/crypto/bcrypt"
)

// User represents a user account
type User struct {
	Username     string    `json:"username"`
	PasswordHash string    `json:"password_hash"`
	CreatedAt    time.Time `json:"created_at"`
	IsAdmin      bool      `json:"is_admin"`
}

// Session represents an active user session
type Session struct {
	Token     string    `json:"token"`
	Username  string    `json:"username"`
	ExpiresAt time.Time `json:"expires_at"`
}

var (
	users       = make(map[string]User)
	sessions    = make(map[string]Session)
	usersMutex  = &sync.RWMutex{}
	sessionsMux = &sync.RWMutex{}
	usersFile   = "./users.json"
	sessionsDir = "./sessions"

	// Session duration: 2 weeks
	sessionDuration = 14 * 24 * time.Hour
)

// InitAuth initializes the authentication system
func InitAuth() error {
	// Create sessions directory if it doesn't exist
	if err := os.MkdirAll(sessionsDir, 0755); err != nil {
		return fmt.Errorf("failed to create sessions directory: %v", err)
	}

	// Load users from file
	if err := loadUsers(); err != nil {
		// If file doesn't exist, create default user
		if os.IsNotExist(err) {
			fmt.Println("No users file found, creating default user 'rams'")
			if err := CreateUser("rams", "forgetit", true); err != nil {
				return fmt.Errorf("failed to create default user: %v", err)
			}
		} else {
			return fmt.Errorf("failed to load users: %v", err)
		}
	}

	// Load sessions from files
	if err := loadSessions(); err != nil {
		fmt.Printf("Warning: failed to load sessions: %v\n", err)
	}

	// Start session cleanup goroutine
	go cleanupExpiredSessions()

	return nil
}

// loadUsers loads users from the JSON file
func loadUsers() error {
	data, err := os.ReadFile(usersFile)
	if err != nil {
		return err
	}

	usersMutex.Lock()
	defer usersMutex.Unlock()

	return json.Unmarshal(data, &users)
}

// saveUsers saves users to the JSON file
func saveUsers() error {
	usersMutex.RLock()
	data, err := json.MarshalIndent(users, "", "  ")
	usersMutex.RUnlock()

	if err != nil {
		return err
	}

	return os.WriteFile(usersFile, data, 0644)
}

// loadSessions loads all sessions from files
func loadSessions() error {
	files, err := os.ReadDir(sessionsDir)
	if err != nil {
		return err
	}

	sessionsMux.Lock()
	defer sessionsMux.Unlock()

	for _, file := range files {
		if filepath.Ext(file.Name()) != ".json" {
			continue
		}

		data, err := os.ReadFile(filepath.Join(sessionsDir, file.Name()))
		if err != nil {
			fmt.Printf("Error reading session file %s: %v\n", file.Name(), err)
			continue
		}

		var session Session
		if err := json.Unmarshal(data, &session); err != nil {
			fmt.Printf("Error parsing session file %s: %v\n", file.Name(), err)
			continue
		}

		// Only load non-expired sessions
		if session.ExpiresAt.After(time.Now()) {
			sessions[session.Token] = session
		} else {
			// Delete expired session file
			os.Remove(filepath.Join(sessionsDir, file.Name()))
		}
	}

	return nil
}

// saveSession saves a session to a file
func saveSession(session Session) error {
	data, err := json.MarshalIndent(session, "", "  ")
	if err != nil {
		return err
	}

	filename := filepath.Join(sessionsDir, session.Token+".json")
	return os.WriteFile(filename, data, 0644)
}

// deleteSessionFile deletes a session file
func deleteSessionFile(token string) error {
	filename := filepath.Join(sessionsDir, token+".json")
	return os.Remove(filename)
}

// CreateUser creates a new user with hashed password
func CreateUser(username, password string, isAdmin bool) error {
	usersMutex.Lock()
	defer usersMutex.Unlock()

	if _, exists := users[username]; exists {
		return fmt.Errorf("user already exists")
	}

	hash, err := bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)
	if err != nil {
		return fmt.Errorf("failed to hash password: %v", err)
	}

	users[username] = User{
		Username:     username,
		PasswordHash: string(hash),
		CreatedAt:    time.Now(),
		IsAdmin:      isAdmin,
	}

	// Save to file (release lock first by using defer-friendly pattern)
	usersMutex.Unlock()
	err = saveUsers()
	usersMutex.Lock()
	return err
}

// DeleteUser removes a user
func DeleteUser(username string) error {
	usersMutex.Lock()
	defer usersMutex.Unlock()

	if _, exists := users[username]; !exists {
		return fmt.Errorf("user not found")
	}

	delete(users, username)

	usersMutex.Unlock()
	err := saveUsers()
	usersMutex.Lock()

	// Also invalidate all sessions for this user
	InvalidateUserSessions(username)

	return err
}

// ChangePassword changes a user's password
func ChangePassword(username, newPassword string) error {
	usersMutex.Lock()
	defer usersMutex.Unlock()

	user, exists := users[username]
	if !exists {
		return fmt.Errorf("user not found")
	}

	hash, err := bcrypt.GenerateFromPassword([]byte(newPassword), bcrypt.DefaultCost)
	if err != nil {
		return fmt.Errorf("failed to hash password: %v", err)
	}

	user.PasswordHash = string(hash)
	users[username] = user

	usersMutex.Unlock()
	err = saveUsers()
	usersMutex.Lock()
	return err
}

// ValidateCredentials checks if username and password are correct
func ValidateCredentials(username, password string) bool {
	usersMutex.RLock()
	user, exists := users[username]
	usersMutex.RUnlock()

	if !exists {
		return false
	}

	err := bcrypt.CompareHashAndPassword([]byte(user.PasswordHash), []byte(password))
	return err == nil
}

// CreateSession creates a new session for a user
func CreateSession(username string) (string, error) {
	// Generate random token
	tokenBytes := make([]byte, 32)
	if _, err := rand.Read(tokenBytes); err != nil {
		return "", fmt.Errorf("failed to generate token: %v", err)
	}
	token := hex.EncodeToString(tokenBytes)

	session := Session{
		Token:     token,
		Username:  username,
		ExpiresAt: time.Now().Add(sessionDuration),
	}

	sessionsMux.Lock()
	sessions[token] = session
	sessionsMux.Unlock()

	// Save to file
	if err := saveSession(session); err != nil {
		return "", fmt.Errorf("failed to save session: %v", err)
	}

	return token, nil
}

// ValidateSession checks if a session token is valid
func ValidateSession(token string) (string, bool) {
	sessionsMux.RLock()
	session, exists := sessions[token]
	sessionsMux.RUnlock()

	if !exists {
		return "", false
	}

	if session.ExpiresAt.Before(time.Now()) {
		// Session expired, remove it
		sessionsMux.Lock()
		delete(sessions, token)
		sessionsMux.Unlock()
		deleteSessionFile(token)
		return "", false
	}

	return session.Username, true
}

// InvalidateSession removes a session
func InvalidateSession(token string) {
	sessionsMux.Lock()
	delete(sessions, token)
	sessionsMux.Unlock()
	deleteSessionFile(token)
}

// InvalidateUserSessions removes all sessions for a user
func InvalidateUserSessions(username string) {
	sessionsMux.Lock()
	defer sessionsMux.Unlock()

	for token, session := range sessions {
		if session.Username == username {
			delete(sessions, token)
			deleteSessionFile(token)
		}
	}
}

// cleanupExpiredSessions periodically removes expired sessions
func cleanupExpiredSessions() {
	ticker := time.NewTicker(1 * time.Hour)
	defer ticker.Stop()

	for range ticker.C {
		sessionsMux.Lock()
		now := time.Now()
		for token, session := range sessions {
			if session.ExpiresAt.Before(now) {
				delete(sessions, token)
				deleteSessionFile(token)
			}
		}
		sessionsMux.Unlock()
	}
}

// GetAllUsers returns all usernames (for sharing UI)
func GetAllUsers() []string {
	usersMutex.RLock()
	defer usersMutex.RUnlock()

	usernames := make([]string, 0, len(users))
	for username := range users {
		usernames = append(usernames, username)
	}
	return usernames
}

// IsAdmin checks if a user is an admin
func IsAdmin(username string) bool {
	usersMutex.RLock()
	defer usersMutex.RUnlock()

	user, exists := users[username]
	if !exists {
		return false
	}
	return user.IsAdmin
}

// AuthMiddleware wraps handlers to require authentication
func AuthMiddleware(next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		cookie, err := r.Cookie("session_token")
		if err != nil {
			http.Redirect(w, r, "/login", http.StatusSeeOther)
			return
		}

		username, valid := ValidateSession(cookie.Value)
		if !valid {
			http.Redirect(w, r, "/login", http.StatusSeeOther)
			return
		}

		// Add username to request context via header (simple approach)
		r.Header.Set("X-Username", username)
		next(w, r)
	}
}

// GetUsernameFromRequest extracts username from authenticated request
func GetUsernameFromRequest(r *http.Request) string {
	// First try header (set by middleware)
	if username := r.Header.Get("X-Username"); username != "" {
		return username
	}

	// Fallback to cookie validation
	cookie, err := r.Cookie("session_token")
	if err != nil {
		return ""
	}

	username, valid := ValidateSession(cookie.Value)
	if !valid {
		return ""
	}
	return username
}

// Device Auth Flow - for CLI/editor clients

// DeviceAuthRequest represents a pending device authentication request
type DeviceAuthRequest struct {
	DeviceCode   string    `json:"device_code"`
	UserCode     string    `json:"user_code"`
	ExpiresAt    time.Time `json:"expires_at"`
	Approved     bool      `json:"approved"`
	Username     string    `json:"username,omitempty"`
	SessionToken string    `json:"session_token,omitempty"`
}

var (
	deviceAuthRequests = make(map[string]*DeviceAuthRequest) // keyed by device_code
	userCodeIndex      = make(map[string]string)             // user_code -> device_code
	deviceAuthMux      = &sync.RWMutex{}
)

// generateUserCode creates a short, user-friendly code
func generateUserCode() string {
	const chars = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789" // no confusing chars
	b := make([]byte, 8)
	rand.Read(b)
	code := make([]byte, 8)
	for i := range code {
		code[i] = chars[int(b[i])%len(chars)]
	}
	// Format as XXXX-XXXX
	return string(code[:4]) + "-" + string(code[4:])
}

// generateDeviceCode creates a longer device code
func generateDeviceCode() string {
	b := make([]byte, 32)
	rand.Read(b)
	return hex.EncodeToString(b)
}

// CreateDeviceAuthRequest creates a new device auth request
func CreateDeviceAuthRequest() *DeviceAuthRequest {
	deviceCode := generateDeviceCode()
	userCode := generateUserCode()

	req := &DeviceAuthRequest{
		DeviceCode: deviceCode,
		UserCode:   userCode,
		ExpiresAt:  time.Now().Add(10 * time.Minute),
		Approved:   false,
	}

	deviceAuthMux.Lock()
	deviceAuthRequests[deviceCode] = req
	userCodeIndex[userCode] = deviceCode
	deviceAuthMux.Unlock()

	return req
}

// GetDeviceAuthByUserCode retrieves a device auth request by user code
func GetDeviceAuthByUserCode(userCode string) *DeviceAuthRequest {
	deviceAuthMux.RLock()
	defer deviceAuthMux.RUnlock()

	deviceCode, exists := userCodeIndex[userCode]
	if !exists {
		return nil
	}

	req, exists := deviceAuthRequests[deviceCode]
	if !exists || req.ExpiresAt.Before(time.Now()) {
		return nil
	}

	return req
}

// GetDeviceAuthByDeviceCode retrieves a device auth request by device code
func GetDeviceAuthByDeviceCode(deviceCode string) *DeviceAuthRequest {
	deviceAuthMux.RLock()
	defer deviceAuthMux.RUnlock()

	req, exists := deviceAuthRequests[deviceCode]
	if !exists || req.ExpiresAt.Before(time.Now()) {
		return nil
	}

	return req
}

// ApproveDeviceAuth approves a device auth request
func ApproveDeviceAuth(userCode, username string) error {
	deviceAuthMux.Lock()
	defer deviceAuthMux.Unlock()

	deviceCode, exists := userCodeIndex[userCode]
	if !exists {
		return fmt.Errorf("invalid code")
	}

	req, exists := deviceAuthRequests[deviceCode]
	if !exists || req.ExpiresAt.Before(time.Now()) {
		return fmt.Errorf("code expired")
	}

	// Create session for the device
	token, err := CreateSession(username)
	if err != nil {
		return err
	}

	req.Approved = true
	req.Username = username
	req.SessionToken = token

	return nil
}

// CleanupExpiredDeviceAuth removes expired device auth requests
func CleanupExpiredDeviceAuth() {
	deviceAuthMux.Lock()
	defer deviceAuthMux.Unlock()

	now := time.Now()
	for deviceCode, req := range deviceAuthRequests {
		if req.ExpiresAt.Before(now) {
			delete(userCodeIndex, req.UserCode)
			delete(deviceAuthRequests, deviceCode)
		}
	}
}

// RemoveDeviceAuth removes a device auth request after it's been used
func RemoveDeviceAuth(deviceCode string) {
	deviceAuthMux.Lock()
	defer deviceAuthMux.Unlock()

	if req, exists := deviceAuthRequests[deviceCode]; exists {
		delete(userCodeIndex, req.UserCode)
		delete(deviceAuthRequests, deviceCode)
	}
}
