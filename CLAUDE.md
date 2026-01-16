# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Procrastinate-Professional is a real-time todo application built with Go backend and vanilla JavaScript frontend. It uses WebSockets for live synchronization across devices and integrates OpenAI for automatic task categorization, tagging, and URL summarization.

## Commands

```bash
# Run the server (starts on port 8090)
go run main.go

# Build the binary
go build -o procrastinate-professional

# Install dependencies
go mod download
```

## Architecture

### Backend (main.go)
- Single-file Go server using gorilla/websocket for real-time communication
- File-based persistence: todos stored as individual JSON files in `./todos/` directory (named by timestamp ID)
- OpenAI integration for:
  - `openai_tag_and_analyze()`: Auto-categorizes todos and extracts tags/due dates using GPT-4o
  - `openai_summarizeurls()`: Summarizes URLs in todo text using GPT-4o with web search
- WebSocket message types: `add_todo`, `update_todo`, `delete_todo` (server responds with `todo_added`, `todo_updated`, `todo_deleted`, `todo_list`)

### Frontend (static/index.html)
- Single HTML file with embedded CSS/JS
- Uses Bootstrap 5 for styling, FullCalendar for calendar view
- WebSocket client auto-reconnects on disconnect
- Features: dark mode, tag cloud filtering, search, tabbed view (active/completed/calendar)

### Data Model
```go
type Todo struct {
    ID        int64    `json:"id"`        // Unix timestamp
    Text      string   `json:"text"`      // May contain HTML after URL processing
    Category  string   `json:"category"`  // AI-assigned: "todo" or "task"
    Tags      []string `json:"tags"`      // AI-extracted tags
    Color     string   `json:"color"`
    Duedate   string   `json:"duedate"`   // AI-extracted, format: YYYY-MM-DD
    Completed bool     `json:"completed"`
}
```

## Environment

Set `OPENAI_API_KEY` in `.env` file for AI features. The app loads this via godotenv.

## Key Dependencies

- `github.com/gorilla/websocket` - WebSocket handling
- `github.com/openai/openai-go` - OpenAI API client
- `github.com/gomarkdown/markdown` - Markdown to HTML conversion
- `mvdan.cc/xurls/v2` - URL extraction from text
