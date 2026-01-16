;;; procrastinate.el --- Emacs client for Procrastinate-Professional -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (websocket "1.14"))
;; Keywords: tools, todo, productivity
;; URL: https://github.com/geckostudios/procrastinate-professional

;;; Commentary:

;; An Emacs client for Procrastinate-Professional, a real-time todo
;; application with WebSocket sync.
;;
;; Features:
;; - Real-time sync via WebSocket
;; - Add, complete, and delete todos
;; - View active and completed tasks
;; - Task sharing support
;;
;; Usage:
;;   (require 'procrastinate)
;;   (setq procrastinate-server-url "ws://localhost:8090/ws")
;;   (setq procrastinate-session-token "your-session-token")
;;   M-x procrastinate-connect
;;   M-x procrastinate-list
;;
;; Get your session token from the browser cookie after logging in.

;;; Code:

(require 'json)
(require 'websocket)
(require 'cl-lib)

;;; Customization

(defgroup procrastinate nil
  "Emacs client for Procrastinate-Professional."
  :group 'tools
  :prefix "procrastinate-")

(defcustom procrastinate-server-url "ws://localhost:8090/ws"
  "WebSocket URL of the Procrastinate-Professional server."
  :type 'string
  :group 'procrastinate)

(defcustom procrastinate-session-token nil
  "Session token for authentication.
Get this from the 'session_token' cookie in your browser after logging in."
  :type '(choice (const nil) string)
  :group 'procrastinate)

(defcustom procrastinate-auto-refresh t
  "Automatically refresh the todo list when updates are received."
  :type 'boolean
  :group 'procrastinate)

;;; Internal Variables

(defvar procrastinate--websocket nil
  "The WebSocket connection to the server.")

(defvar procrastinate--todos '()
  "List of all todos from the server.")

(defvar procrastinate--users '()
  "List of all users from the server.")

(defvar procrastinate--current-user nil
  "Current logged-in username.")

(defvar procrastinate--connected nil
  "Whether we are currently connected to the server.")

(defvar procrastinate--reconnect-timer nil
  "Timer for reconnection attempts.")

;;; Faces

(defface procrastinate-completed-face
  '((t :strike-through t :foreground "gray50"))
  "Face for completed todos."
  :group 'procrastinate)

(defface procrastinate-category-face
  '((t :foreground "purple" :weight bold))
  "Face for todo categories."
  :group 'procrastinate)

(defface procrastinate-tag-face
  '((t :foreground "blue"))
  "Face for todo tags."
  :group 'procrastinate)

(defface procrastinate-duedate-face
  '((t :foreground "orange"))
  "Face for todo due dates."
  :group 'procrastinate)

(defface procrastinate-shared-face
  '((t :foreground "teal"))
  "Face for shared todo indicators."
  :group 'procrastinate)

(defface procrastinate-owner-face
  '((t :foreground "green"))
  "Face for todo owner."
  :group 'procrastinate)

;;; WebSocket Connection

(defun procrastinate--make-cookie-header ()
  "Create the cookie header for authentication."
  (when procrastinate-session-token
    (format "session_token=%s" procrastinate-session-token)))

(defun procrastinate-connect ()
  "Connect to the Procrastinate-Professional server."
  (interactive)
  (when procrastinate--websocket
    (websocket-close procrastinate--websocket))
  (unless procrastinate-session-token
    (error "Please set `procrastinate-session-token' before connecting"))
  (condition-case err
      (setq procrastinate--websocket
            (websocket-open
             procrastinate-server-url
             :custom-header-alist
             `(("Cookie" . ,(procrastinate--make-cookie-header)))
             :on-open #'procrastinate--on-open
             :on-message #'procrastinate--on-message
             :on-close #'procrastinate--on-close
             :on-error #'procrastinate--on-error))
    (error
     (message "Procrastinate: Failed to connect: %s" (error-message-string err))
     (procrastinate--schedule-reconnect))))

(defun procrastinate-disconnect ()
  "Disconnect from the Procrastinate-Professional server."
  (interactive)
  (when procrastinate--reconnect-timer
    (cancel-timer procrastinate--reconnect-timer)
    (setq procrastinate--reconnect-timer nil))
  (when procrastinate--websocket
    (websocket-close procrastinate--websocket)
    (setq procrastinate--websocket nil))
  (setq procrastinate--connected nil)
  (message "Procrastinate: Disconnected"))

(defun procrastinate--schedule-reconnect ()
  "Schedule a reconnection attempt."
  (unless procrastinate--reconnect-timer
    (setq procrastinate--reconnect-timer
          (run-with-timer 5 nil
                          (lambda ()
                            (setq procrastinate--reconnect-timer nil)
                            (message "Procrastinate: Attempting to reconnect...")
                            (procrastinate-connect))))))

(defun procrastinate--on-open (_websocket)
  "Handle WebSocket connection open."
  (setq procrastinate--connected t)
  (message "Procrastinate: Connected to server"))

(defun procrastinate--on-message (_websocket frame)
  "Handle incoming WebSocket message in FRAME."
  (let* ((payload (websocket-frame-text frame))
         (data (json-read-from-string payload))
         (msg-type (alist-get 'type data)))
    (cond
     ((string= msg-type "todo_list")
      (setq procrastinate--todos (append (alist-get 'todos data) nil))
      (setq procrastinate--users (append (alist-get 'users data) nil))
      (when-let ((username (alist-get 'username data)))
        (setq procrastinate--current-user username))
      (when procrastinate-auto-refresh
        (procrastinate--refresh-buffer)))

     ((string= msg-type "todo_added")
      (let ((todo (alist-get 'todo data)))
        (push todo procrastinate--todos)
        (when procrastinate-auto-refresh
          (procrastinate--refresh-buffer))
        (message "Procrastinate: Todo added")))

     ((string= msg-type "todo_updated")
      (let ((todo (alist-get 'todo data)))
        (setq procrastinate--todos
              (mapcar (lambda (t)
                        (if (= (alist-get 'id t) (alist-get 'id todo))
                            todo
                          t))
                      procrastinate--todos))
        (when procrastinate-auto-refresh
          (procrastinate--refresh-buffer))))

     ((string= msg-type "todo_deleted")
      (let ((todo-id (alist-get 'todoId data)))
        (setq procrastinate--todos
              (cl-remove-if (lambda (t) (= (alist-get 'id t) todo-id))
                            procrastinate--todos))
        (when procrastinate-auto-refresh
          (procrastinate--refresh-buffer)))))))

(defun procrastinate--on-close (_websocket)
  "Handle WebSocket connection close."
  (setq procrastinate--connected nil)
  (setq procrastinate--websocket nil)
  (message "Procrastinate: Connection closed")
  (procrastinate--schedule-reconnect))

(defun procrastinate--on-error (_websocket _type err)
  "Handle WebSocket error ERR."
  (message "Procrastinate: WebSocket error: %s" err))

(defun procrastinate--send (data)
  "Send DATA as JSON to the server."
  (unless procrastinate--connected
    (error "Not connected to server. Run M-x procrastinate-connect"))
  (websocket-send-text procrastinate--websocket (json-encode data)))

;;; Todo Operations

(defun procrastinate-add (text)
  "Add a new todo with TEXT."
  (interactive "sTodo: ")
  (let ((todo `((id . ,(round (* (float-time) 1000)))
                (text . ,text)
                (completed . :json-false)
                (owner . ,procrastinate--current-user)
                (shared_with . []))))
    (procrastinate--send `((type . "add_todo")
                           (todo . ,todo)))))

(defun procrastinate-toggle (todo-id)
  "Toggle the completed status of todo with TODO-ID."
  (interactive)
  (let ((todo (cl-find-if (lambda (t) (= (alist-get 'id t) todo-id))
                          procrastinate--todos)))
    (when todo
      (let* ((completed (alist-get 'completed todo))
             (new-completed (if (eq completed :json-false) t :json-false)))
        (setf (alist-get 'completed todo) new-completed)
        (procrastinate--send `((type . "update_todo")
                               (todo . ,todo)))))))

(defun procrastinate-delete (todo-id)
  "Delete the todo with TODO-ID."
  (interactive)
  (when (yes-or-no-p "Delete this todo? ")
    (procrastinate--send `((type . "delete_todo")
                           (todoId . ,todo-id)))))

;;; Buffer Display

(defvar procrastinate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'procrastinate-add)
    (define-key map (kbd "RET") #'procrastinate-toggle-at-point)
    (define-key map (kbd "SPC") #'procrastinate-toggle-at-point)
    (define-key map (kbd "d") #'procrastinate-delete-at-point)
    (define-key map (kbd "g") #'procrastinate-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "c") #'procrastinate-show-completed)
    (define-key map (kbd "A") #'procrastinate-show-active)
    map)
  "Keymap for `procrastinate-mode'.")

(define-derived-mode procrastinate-mode special-mode "Procrastinate"
  "Major mode for viewing and managing Procrastinate todos.

\\{procrastinate-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defvar-local procrastinate--show-completed nil
  "Whether to show completed todos in the current buffer.")

(defun procrastinate-list ()
  "Open the Procrastinate todo list buffer."
  (interactive)
  (unless procrastinate--connected
    (procrastinate-connect)
    (sit-for 1))
  (let ((buf (get-buffer-create "*Procrastinate*")))
    (with-current-buffer buf
      (procrastinate-mode)
      (procrastinate--render-todos))
    (switch-to-buffer buf)))

(defun procrastinate-show-completed ()
  "Show completed todos."
  (interactive)
  (setq procrastinate--show-completed t)
  (procrastinate--render-todos))

(defun procrastinate-show-active ()
  "Show active todos."
  (interactive)
  (setq procrastinate--show-completed nil)
  (procrastinate--render-todos))

(defun procrastinate--refresh-buffer ()
  "Refresh the Procrastinate buffer if it exists."
  (when-let ((buf (get-buffer "*Procrastinate*")))
    (with-current-buffer buf
      (procrastinate--render-todos))))

(defun procrastinate-refresh ()
  "Refresh the todo list."
  (interactive)
  (procrastinate--render-todos))

(defun procrastinate--render-todos ()
  "Render the todos in the current buffer."
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (insert (propertize "Procrastinate Professional\n"
                        'face '(:weight bold :height 1.3)))
    (insert (format "Connected as: %s\n" (or procrastinate--current-user "unknown")))
    (insert (make-string 40 ?-) "\n\n")

    (if procrastinate--show-completed
        (progn
          (insert (propertize "Completed Tasks\n" 'face '(:weight bold)))
          (insert "[Press 'A' for active tasks]\n\n"))
      (progn
        (insert (propertize "Active Tasks\n" 'face '(:weight bold)))
        (insert "[Press 'c' for completed tasks]\n\n")))

    (let ((filtered-todos
           (cl-remove-if-not
            (lambda (todo)
              (let ((completed (alist-get 'completed todo)))
                (if procrastinate--show-completed
                    (not (eq completed :json-false))
                  (eq completed :json-false))))
            procrastinate--todos)))
      (if (null filtered-todos)
          (insert (propertize "  No todos.\n" 'face 'font-lock-comment-face))
        (dolist (todo filtered-todos)
          (procrastinate--render-todo todo))))

    (insert "\n" (make-string 40 ?-) "\n")
    (insert "Keys: [a]dd  [RET/SPC]toggle  [d]elete  [g]refresh  [q]uit\n")
    (goto-char (min pos (point-max)))))

(defun procrastinate--render-todo (todo)
  "Render a single TODO item."
  (let* ((id (alist-get 'id todo))
         (text (alist-get 'text todo))
         (completed (not (eq (alist-get 'completed todo) :json-false)))
         (category (alist-get 'category todo))
         (tags (alist-get 'tags todo))
         (duedate (alist-get 'duedate todo))
         (owner (alist-get 'owner todo))
         (shared-with (alist-get 'shared_with todo))
         (is-shared (and owner (not (string= owner procrastinate--current-user))))
         (clean-text (replace-regexp-in-string "<[^>]*>" "" (or text "")))
         (checkbox (if completed "[x]" "[ ]"))
         (start (point)))

    ;; Checkbox and text
    (insert (format "  %s " checkbox))
    (if completed
        (insert (propertize clean-text 'face 'procrastinate-completed-face))
      (insert clean-text))
    (insert "\n")

    ;; Metadata line
    (insert "      ")
    (when category
      (insert (propertize (format "[%s]" category) 'face 'procrastinate-category-face) " "))
    (when (and tags (> (length tags) 0))
      (dolist (tag (append tags nil))
        (insert (propertize (format "#%s" tag) 'face 'procrastinate-tag-face) " ")))
    (when duedate
      (insert (propertize (format "due:%s" duedate) 'face 'procrastinate-duedate-face) " "))
    (when is-shared
      (insert (propertize (format "from:%s" owner) 'face 'procrastinate-shared-face) " "))
    (when (and shared-with (> (length shared-with) 0) (not is-shared))
      (insert (propertize (format "shared:%d" (length shared-with)) 'face 'procrastinate-owner-face) " "))
    (insert "\n")

    ;; Store todo ID as text property
    (put-text-property start (point) 'procrastinate-todo-id id)))

(defun procrastinate--get-todo-at-point ()
  "Get the todo ID at point."
  (get-text-property (point) 'procrastinate-todo-id))

(defun procrastinate-toggle-at-point ()
  "Toggle the todo at point."
  (interactive)
  (if-let ((todo-id (procrastinate--get-todo-at-point)))
      (procrastinate-toggle todo-id)
    (message "No todo at point")))

(defun procrastinate-delete-at-point ()
  "Delete the todo at point."
  (interactive)
  (if-let ((todo-id (procrastinate--get-todo-at-point)))
      (procrastinate-delete todo-id)
    (message "No todo at point")))

;;; Quick Add

(defun procrastinate-quick-add ()
  "Quickly add a todo from anywhere."
  (interactive)
  (unless procrastinate--connected
    (procrastinate-connect)
    (sit-for 1))
  (let ((text (read-string "Quick todo: ")))
    (when (not (string-empty-p text))
      (procrastinate-add text))))

;;; Provide

(provide 'procrastinate)

;;; procrastinate.el ends here
