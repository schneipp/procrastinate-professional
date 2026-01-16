# Procrastinate-Professional

## TL;DR

You know that thing you *should* be doing, but you're not because... Youtube exists? Meet **Procrastinate-Professional**: the ultimate tool to *organize* your procrastination like a true professional. We delay, you slay the doing. Either that or we distract you while you're stuck.

---

## Q&A

### Q: Why another todo app?

A: i hate ms todo and no other tool uses websockets. i want live updates. if i add a todo on my pc, i want it to appear on my tablet in the exact same second. i don't want to wait for a sync. i want it now. i want it live. i want it in my face. this is the only way to increase the procrastination level to the maximum. i want to see my tasks in real time, so i can ignore them in real time. Also it uses AI!

### Set your OPENAI_API_KEY environment variable to use the AI features

---

## Why use this?

- **Productivity theater**: Appear productive while boldly avoiding actual work. Bonus points for convincing yourself you're "planning."
- **Guilt-free delay**: Turn unfinished tasks into *pending responsibilities*. Feels more professional.  

---

## Features

- **Websockets**: Real-time sync across all your devices. Add a todo on your phone, see it on your laptop instantly.
- **AI-powered**: Automatic categorization, tagging, and URL summarization using OpenAI.
- **Calendar view**: Drag and drop tasks onto the calendar. See your procrastination schedule at a glance.
- **Tag cloud**: Filter tasks by tags. Organize your chaos.
- **Dark mode**: Because we're professionals.
- **User authentication**: Login system with session management (2-week sessions).
- **Multi-user support**: Create users, manage accounts, share tasks with others.
- **Task sharing**: Share individual tasks with other users. Collaborate on procrastination.

---

## Getting Started

### Prerequisites

- Go 1.22+
- OpenAI API key (for AI features)

### Installation

```bash
# Clone the repo
git clone https://github.com/geckostudios/procrastinate-professional.git
cd procrastinate-professional

# Install dependencies
go mod download

# Create .env file with your OpenAI API key
echo "OPENAI_API_KEY=your-key-here" > .env

# Run the server
go run main.go
```

The server starts on port 8090. Navigate to `http://localhost:8090`.

### Default User

On first run, a default admin user is created:
- **Username**: `rams`
- **Password**: `forgetit`

You can create additional users from the user menu (admin only).

---

## Architecture

- **Backend**: Single Go binary using gorilla/websocket
- **Frontend**: Vanilla JS with Bootstrap 5 and FullCalendar
- **Storage**: File-based JSON storage (no database needed)
  - `./todos/` - Todo items (one JSON file per todo)
  - `./users.json` - User accounts
  - `./sessions/` - Active sessions

---

## User Management

- Admins can create/delete users from the UI
- Users can change their own password
- Sessions last 2 weeks before requiring re-login

## Task Sharing

- Click the share button on any task you own
- Select users to share with
- Shared users can view and complete tasks
- Only the owner can delete a task

---

## Security Note

This was built for internal use behind a VPN. If you're exposing it to the internet, consider:
- Using HTTPS/WSS
- Adding rate limiting
- Implementing proper CSRF protection
