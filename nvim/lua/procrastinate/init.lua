-- procrastinate.nvim
-- Neovim client for Procrastinate-Professional

local M = {}

-- Configuration
M.config = {
  server_url = nil,
  session_token = nil,
  width = 0.8,
  height = 0.8,
  border = "rounded",
  config_path = vim.fn.stdpath("data") .. "/procrastinate.json",
}

-- State
local state = {
  todos = {},
  users = {},
  current_user = nil,
  connected = false,
  buf = nil,
  win = nil,
  show_completed = false,
  ws_job = nil,        -- WebSocket job ID
  ws_stdin = nil,      -- Stdin pipe for sending messages
  pending_callbacks = {}, -- Callbacks waiting for responses
}

-- Icons
local icons = {
  unchecked = "",
  checked = "",
  tag = "",
  calendar = "",
  user = "",
  shared = "",
  category = "",
}

-- Highlights
local function setup_highlights()
  vim.api.nvim_set_hl(0, "ProcrastinateTitle", { fg = "#b388ff", bold = true })
  vim.api.nvim_set_hl(0, "ProcrastinateHeader", { fg = "#82aaff", bold = true })
  vim.api.nvim_set_hl(0, "ProcrastinateUnchecked", { fg = "#c792ea" })
  vim.api.nvim_set_hl(0, "ProcrastinateChecked", { fg = "#6a6a6a" })
  vim.api.nvim_set_hl(0, "ProcrastinateCompleted", { fg = "#6a6a6a", strikethrough = true })
  vim.api.nvim_set_hl(0, "ProcrastinateTag", { fg = "#89ddff" })
  vim.api.nvim_set_hl(0, "ProcrastinateCategory", { fg = "#c792ea" })
  vim.api.nvim_set_hl(0, "ProcrastinateDue", { fg = "#ffcb6b" })
  vim.api.nvim_set_hl(0, "ProcrastinateShared", { fg = "#80cbc4" })
  vim.api.nvim_set_hl(0, "ProcrastinateOwner", { fg = "#c3e88d" })
  vim.api.nvim_set_hl(0, "ProcrastinateHelp", { fg = "#6a6a6a", italic = true })
  vim.api.nvim_set_hl(0, "ProcrastinateBorder", { fg = "#b388ff" })
  vim.api.nvim_set_hl(0, "ProcrastinateNormal", { bg = "#1e1e2e" })
  vim.api.nvim_set_hl(0, "ProcrastinateCode", { fg = "#ffcb6b", bold = true })
  vim.api.nvim_set_hl(0, "ProcrastinateUrl", { fg = "#82aaff", underline = true })
end

-- Load saved config
local function load_config()
  local path = M.config.config_path
  local file = io.open(path, "r")
  if file then
    local content = file:read("*a")
    file:close()
    local ok, data = pcall(vim.json.decode, content)
    if ok and data then
      if data.server_url then
        M.config.server_url = data.server_url
      end
      if data.session_token then
        M.config.session_token = data.session_token
      end
    end
  end
end

-- Save config
local function save_config()
  local path = M.config.config_path
  local data = {
    server_url = M.config.server_url,
    session_token = M.config.session_token,
  }
  local file = io.open(path, "w")
  if file then
    file:write(vim.json.encode(data))
    file:close()
  end
end

-- Strip HTML tags
local function strip_html(str)
  if not str then return "" end
  return str:gsub("<[^>]+>", "")
end

-- Get todo at cursor
local function get_todo_at_cursor()
  if not state.win or not vim.api.nvim_win_is_valid(state.win) then
    return nil
  end
  local line = vim.api.nvim_win_get_cursor(state.win)[1]
  local todo_lines = vim.b[state.buf].todo_lines
  local todo_index = todo_lines and todo_lines[line]
  if todo_index then
    return state.todos[todo_index]
  end
  return nil
end

-- Get base URL from server URL
local function get_base_url()
  if not M.config.server_url then return nil end
  return M.config.server_url:gsub("ws://", "http://"):gsub("wss://", "https://"):gsub("/ws$", "")
end

-- Handle incoming WebSocket message
local function handle_ws_message(data)
  local ok, json = pcall(vim.json.decode, data)
  if not ok then return end

  if json.type == "todo_list" then
    state.todos = json.todos or {}
    state.users = json.users or {}
    state.current_user = json.username
    state.connected = true
    vim.schedule(function()
      if state.buf and vim.api.nvim_buf_is_valid(state.buf) then
        render()
      end
    end)
  elseif json.type == "todo_added" then
    -- Update local state
    local found = false
    for i, t in ipairs(state.todos) do
      if t.id == json.todo.id then
        state.todos[i] = json.todo
        found = true
        break
      end
    end
    if not found then
      table.insert(state.todos, json.todo)
    end
    vim.schedule(function()
      if state.buf and vim.api.nvim_buf_is_valid(state.buf) then
        render()
      end
      vim.notify("Procrastinate: Todo added", vim.log.levels.INFO)
    end)
  elseif json.type == "todo_updated" then
    for i, t in ipairs(state.todos) do
      if t.id == json.todo.id then
        state.todos[i] = json.todo
        break
      end
    end
    vim.schedule(function()
      if state.buf and vim.api.nvim_buf_is_valid(state.buf) then
        render()
      end
    end)
  elseif json.type == "todo_deleted" then
    for i, t in ipairs(state.todos) do
      if t.id == json.todoId then
        table.remove(state.todos, i)
        break
      end
    end
    vim.schedule(function()
      if state.buf and vim.api.nvim_buf_is_valid(state.buf) then
        render()
      end
    end)
  end
end

-- Connect to WebSocket server
local function ws_connect(callback)
  if state.ws_job then
    -- Already connected
    if callback then callback() end
    return
  end

  if not M.config.session_token then
    vim.notify("Procrastinate: Not authenticated. Run :ProcrastinateAuth", vim.log.levels.ERROR)
    return
  end

  if not M.config.server_url then
    vim.notify("Procrastinate: No server configured. Run :ProcrastinateAuth", vim.log.levels.ERROR)
    return
  end

  local stdin = vim.loop.new_pipe(false)
  local stdout = vim.loop.new_pipe(false)
  local stderr = vim.loop.new_pipe(false)

  state.ws_stdin = stdin

  local handle, pid
  handle, pid = vim.loop.spawn("websocat", {
    args = {
      "-H", "Cookie: session_token=" .. M.config.session_token,
      M.config.server_url
    },
    stdio = { stdin, stdout, stderr }
  }, function(code, signal)
    -- On exit
    vim.schedule(function()
      state.connected = false
      state.ws_job = nil
      state.ws_stdin = nil
      if code ~= 0 then
        vim.notify("Procrastinate: WebSocket disconnected", vim.log.levels.WARN)
      end
    end)
    stdin:close()
    stdout:close()
    stderr:close()
    if handle then handle:close() end
  end)

  if not handle then
    vim.notify("Procrastinate: Failed to start websocat. Is it installed?", vim.log.levels.ERROR)
    return
  end

  state.ws_job = { handle = handle, pid = pid }

  -- Read stdout (incoming messages)
  local buffer = ""
  stdout:read_start(function(err, data)
    if err then
      vim.schedule(function()
        vim.notify("Procrastinate: Read error: " .. err, vim.log.levels.ERROR)
      end)
      return
    end
    if data then
      buffer = buffer .. data
      -- Process complete lines
      while true do
        local newline = buffer:find("\n")
        if not newline then break end
        local line = buffer:sub(1, newline - 1)
        buffer = buffer:sub(newline + 1)
        if line ~= "" then
          handle_ws_message(line)
        end
      end
    end
  end)

  -- Read stderr
  stderr:read_start(function(err, data)
    if data and data ~= "" then
      vim.schedule(function()
        -- Only show errors, not connection messages
        if data:find("error") or data:find("Error") then
          vim.notify("Procrastinate: " .. data, vim.log.levels.ERROR)
        end
      end)
    end
  end)

  state.connected = true
  if callback then
    -- Give it a moment to receive initial todo_list
    vim.defer_fn(callback, 500)
  end
end

-- Disconnect WebSocket
local function ws_disconnect()
  if state.ws_stdin then
    state.ws_stdin:close()
    state.ws_stdin = nil
  end
  if state.ws_job and state.ws_job.handle then
    state.ws_job.handle:kill(15) -- SIGTERM
    state.ws_job = nil
  end
  state.connected = false
end

-- Send message via WebSocket
local function ws_send(msg)
  if not state.ws_stdin or not state.connected then
    vim.notify("Procrastinate: Not connected", vim.log.levels.ERROR)
    return false
  end

  local json_msg = vim.json.encode(msg) .. "\n"
  state.ws_stdin:write(json_msg)
  return true
end

-- Render the todo list
function render()
  if not state.buf or not vim.api.nvim_buf_is_valid(state.buf) then
    return
  end

  vim.api.nvim_buf_set_option(state.buf, "modifiable", true)

  local lines = {}
  local highlights = {}
  local todo_lines = {}

  -- Title
  table.insert(lines, "")
  table.insert(lines, "  ╭─────────────────────────────────────╮")
  table.insert(lines, "  │     Procrastinate Professional     │")
  table.insert(lines, "  ╰─────────────────────────────────────╯")
  table.insert(lines, "")
  table.insert(highlights, { line = 2, col = 0, end_col = -1, hl = "ProcrastinateTitle" })
  table.insert(highlights, { line = 3, col = 0, end_col = -1, hl = "ProcrastinateTitle" })
  table.insert(highlights, { line = 4, col = 0, end_col = -1, hl = "ProcrastinateTitle" })

  -- Connection status
  local conn_icon = state.connected and "●" or "○"
  local conn_color = state.connected and "ProcrastinateOwner" or "ProcrastinateDue"
  local status = string.format("  %s Connected as: %s", conn_icon, state.current_user or "unknown")
  table.insert(lines, status)
  table.insert(highlights, { line = #lines, col = 2, end_col = 3, hl = conn_color })
  table.insert(lines, "")

  -- Section header
  local header = state.show_completed and "  ══════ Completed Tasks ══════" or "  ══════ Active Tasks ══════"
  table.insert(lines, header)
  table.insert(highlights, { line = #lines, col = 0, end_col = -1, hl = "ProcrastinateHeader" })
  table.insert(lines, "")

  -- Filter todos
  local filtered = {}
  for _, todo in ipairs(state.todos) do
    local completed = todo.completed == true
    if state.show_completed == completed then
      table.insert(filtered, todo)
    end
  end

  if #filtered == 0 then
    table.insert(lines, "  No todos here. Time to procrastinate!")
    table.insert(highlights, { line = #lines, col = 0, end_col = -1, hl = "ProcrastinateHelp" })
  else
    for i, todo in ipairs(filtered) do
      local line_num = #lines + 1

      local completed = todo.completed == true
      local icon = completed and icons.checked or icons.unchecked
      local text = strip_html(todo.text or "")
      if #text > 50 then
        text = text:sub(1, 47) .. "..."
      end

      local todo_line = string.format("  %s %s", icon, text)
      table.insert(lines, todo_line)

      if completed then
        table.insert(highlights, { line = line_num, col = 0, end_col = 4, hl = "ProcrastinateChecked" })
        table.insert(highlights, { line = line_num, col = 4, end_col = -1, hl = "ProcrastinateCompleted" })
      else
        table.insert(highlights, { line = line_num, col = 0, end_col = 4, hl = "ProcrastinateUnchecked" })
      end

      -- Metadata line
      local meta = "    "
      local meta_hls = {}

      if todo.category and todo.category ~= "" then
        local start = #meta
        meta = meta .. icons.category .. " " .. todo.category .. "  "
        table.insert(meta_hls, { start = start, finish = #meta, hl = "ProcrastinateCategory" })
      end

      if todo.tags and #todo.tags > 0 then
        for _, tag in ipairs(todo.tags) do
          local start = #meta
          meta = meta .. icons.tag .. tag .. " "
          table.insert(meta_hls, { start = start, finish = #meta, hl = "ProcrastinateTag" })
        end
      end

      if todo.duedate and todo.duedate ~= "" then
        local start = #meta
        meta = meta .. icons.calendar .. " " .. todo.duedate .. "  "
        table.insert(meta_hls, { start = start, finish = #meta, hl = "ProcrastinateDue" })
      end

      if todo.owner and todo.owner ~= state.current_user then
        local start = #meta
        meta = meta .. icons.user .. " " .. todo.owner .. "  "
        table.insert(meta_hls, { start = start, finish = #meta, hl = "ProcrastinateShared" })
      end

      if todo.shared_with and #todo.shared_with > 0 and todo.owner == state.current_user then
        local start = #meta
        meta = meta .. icons.shared .. " shared:" .. #todo.shared_with
        table.insert(meta_hls, { start = start, finish = #meta, hl = "ProcrastinateOwner" })
      end

      if #meta > 4 then
        table.insert(lines, meta)
        local meta_line = #lines
        for _, mhl in ipairs(meta_hls) do
          table.insert(highlights, { line = meta_line, col = mhl.start, end_col = mhl.finish, hl = mhl.hl })
        end
      end

      -- Store todo reference (use actual index in state.todos)
      local actual_index
      for idx, t in ipairs(state.todos) do
        if t.id == todo.id then
          actual_index = idx
          break
        end
      end
      todo_lines[line_num] = actual_index
      -- Also mark metadata line with same todo
      if #meta > 4 then
        todo_lines[#lines] = actual_index
      end
    end
  end

  -- Help footer
  table.insert(lines, "")
  table.insert(lines, "  ─────────────────────────────────────")
  local help = "  a:add  ⏎:toggle  d:delete  c:completed  r:refresh  q:quit"
  table.insert(lines, help)
  table.insert(highlights, { line = #lines, col = 0, end_col = -1, hl = "ProcrastinateHelp" })
  table.insert(lines, "")

  -- Set lines
  vim.api.nvim_buf_set_lines(state.buf, 0, -1, false, lines)

  -- Apply highlights
  for _, hl in ipairs(highlights) do
    vim.api.nvim_buf_add_highlight(state.buf, -1, hl.hl, hl.line - 1, hl.col, hl.end_col)
  end

  -- Store todo line mapping
  vim.b[state.buf].todo_lines = todo_lines

  vim.api.nvim_buf_set_option(state.buf, "modifiable", false)
end

-- Create floating window
local function create_window()
  local width = math.floor(vim.o.columns * M.config.width)
  local height = math.floor(vim.o.lines * M.config.height)
  local row = math.floor((vim.o.lines - height) / 2)
  local col = math.floor((vim.o.columns - width) / 2)

  -- Create buffer
  state.buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_option(state.buf, "bufhidden", "wipe")
  vim.api.nvim_buf_set_option(state.buf, "filetype", "procrastinate")

  -- Create window
  state.win = vim.api.nvim_open_win(state.buf, true, {
    relative = "editor",
    width = width,
    height = height,
    row = row,
    col = col,
    style = "minimal",
    border = M.config.border,
    title = " Procrastinate ",
    title_pos = "center",
  })

  -- Window options
  vim.api.nvim_win_set_option(state.win, "winhl", "Normal:ProcrastinateNormal,FloatBorder:ProcrastinateBorder")
  vim.api.nvim_win_set_option(state.win, "cursorline", true)

  -- Keymaps
  local opts = { buffer = state.buf, silent = true }
  vim.keymap.set("n", "q", M.close, opts)
  vim.keymap.set("n", "<Esc>", M.close, opts)
  vim.keymap.set("n", "a", M.add_todo, opts)
  vim.keymap.set("n", "<CR>", M.toggle_todo, opts)
  vim.keymap.set("n", "<Space>", M.toggle_todo, opts)
  vim.keymap.set("n", "d", M.delete_todo, opts)
  vim.keymap.set("n", "r", M.refresh, opts)
  vim.keymap.set("n", "c", M.toggle_view, opts)
end

-- Auth floating window
local function create_auth_window()
  local width = 60
  local height = 16
  local row = math.floor((vim.o.lines - height) / 2)
  local col = math.floor((vim.o.columns - width) / 2)

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    row = row,
    col = col,
    style = "minimal",
    border = "rounded",
    title = " Authenticate ",
    title_pos = "center",
  })

  vim.api.nvim_win_set_option(win, "winhl", "Normal:ProcrastinateNormal,FloatBorder:ProcrastinateBorder")

  return buf, win
end

-- Device auth flow
function M.auth()
  -- Ask for server URL if not set
  local default_url = M.config.server_url
  if default_url then
    default_url = default_url:gsub("/ws$", "")
  end

  vim.ui.input({
    prompt = "Server URL (e.g., http://localhost:8090): ",
    default = default_url or "http://localhost:8090",
  }, function(url)
    if not url or url == "" then
      vim.notify("Procrastinate: Auth cancelled", vim.log.levels.WARN)
      return
    end

    -- Normalize URL
    url = url:gsub("/$", ""):gsub("/ws$", "")
    M.config.server_url = url .. "/ws"
    local base_url = url

    -- Request device code
    local cmd = string.format('curl -s -X POST "%s/api/device/code"', base_url)

    vim.fn.jobstart(cmd, {
      stdout_buffered = true,
      on_stdout = function(_, data)
        if not data or not data[1] or data[1] == "" then
          vim.schedule(function()
            vim.notify("Procrastinate: Failed to connect to server", vim.log.levels.ERROR)
          end)
          return
        end

        local ok, response = pcall(vim.json.decode, data[1])
        if not ok or not response.user_code then
          vim.schedule(function()
            vim.notify("Procrastinate: Invalid server response", vim.log.levels.ERROR)
          end)
          return
        end

        local device_code = response.device_code
        local user_code = response.user_code
        local auth_url = base_url .. "/auth/device"

        -- Show auth window
        vim.schedule(function()
          local buf, win = create_auth_window()

          local lines = {
            "",
            "  ╭────────────────────────────────────────────────────╮",
            "  │           Device Authentication                   │",
            "  ╰────────────────────────────────────────────────────╯",
            "",
            "  1. Open this URL in your browser:",
            "",
            "     " .. auth_url,
            "",
            "  2. Enter this code:",
            "",
            "     " .. user_code,
            "",
            "  Waiting for authorization...",
            "",
            "  Press 'q' to cancel",
          }

          vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

          -- Highlights
          vim.api.nvim_buf_add_highlight(buf, -1, "ProcrastinateTitle", 1, 0, -1)
          vim.api.nvim_buf_add_highlight(buf, -1, "ProcrastinateTitle", 2, 0, -1)
          vim.api.nvim_buf_add_highlight(buf, -1, "ProcrastinateTitle", 3, 0, -1)
          vim.api.nvim_buf_add_highlight(buf, -1, "ProcrastinateUrl", 7, 5, -1)
          vim.api.nvim_buf_add_highlight(buf, -1, "ProcrastinateCode", 11, 5, -1)
          vim.api.nvim_buf_add_highlight(buf, -1, "ProcrastinateHelp", 13, 0, -1)

          vim.api.nvim_buf_set_option(buf, "modifiable", false)

          -- Poll for approval
          local poll_count = 0
          local max_polls = 120 -- 10 minutes at 5 second intervals
          local timer = nil

          local function cleanup()
            if timer then
              timer:stop()
              timer:close()
              timer = nil
            end
            if vim.api.nvim_win_is_valid(win) then
              vim.api.nvim_win_close(win, true)
            end
          end

          -- Cancel keymap
          vim.keymap.set("n", "q", cleanup, { buffer = buf, silent = true })
          vim.keymap.set("n", "<Esc>", cleanup, { buffer = buf, silent = true })

          local function poll()
            if poll_count >= max_polls then
              vim.schedule(function()
                cleanup()
                vim.notify("Procrastinate: Auth timeout", vim.log.levels.ERROR)
              end)
              return
            end

            poll_count = poll_count + 1

            local poll_cmd = string.format(
              'curl -s -X POST -H "Content-Type: application/json" -d \'{"device_code":"%s"}\' "%s/api/device/token"',
              device_code,
              base_url
            )

            vim.fn.jobstart(poll_cmd, {
              stdout_buffered = true,
              on_stdout = function(_, poll_data)
                if not poll_data or not poll_data[1] then return end

                local poll_ok, poll_response = pcall(vim.json.decode, poll_data[1])
                if not poll_ok then return end

                if poll_response.access_token then
                  -- Success!
                  M.config.session_token = poll_response.access_token
                  save_config()

                  vim.schedule(function()
                    cleanup()
                    vim.notify("Procrastinate: Authenticated as " .. (poll_response.username or "user"), vim.log.levels.INFO)
                  end)
                elseif poll_response.error == "expired_token" then
                  vim.schedule(function()
                    cleanup()
                    vim.notify("Procrastinate: Auth code expired", vim.log.levels.ERROR)
                  end)
                end
                -- If "authorization_pending", keep polling
              end,
            })
          end

          -- Start polling
          timer = vim.loop.new_timer()
          timer:start(0, 5000, vim.schedule_wrap(poll))
        end)
      end,
    })
  end)
end

-- Public functions

function M.setup(opts)
  M.config = vim.tbl_deep_extend("force", M.config, opts or {})
  setup_highlights()
  load_config()

  -- Set up the keybinding
  vim.keymap.set("n", "<leader>nrn", M.open, { desc = "Procrastinate: Open todo list" })
end

function M.open()
  -- Check if authenticated
  if not M.config.session_token or not M.config.server_url then
    vim.notify("Procrastinate: Not authenticated. Run :ProcrastinateAuth", vim.log.levels.WARN)
    M.auth()
    return
  end

  if state.win and vim.api.nvim_win_is_valid(state.win) then
    vim.api.nvim_set_current_win(state.win)
    return
  end

  create_window()

  -- Show loading
  vim.api.nvim_buf_set_option(state.buf, "modifiable", true)
  vim.api.nvim_buf_set_lines(state.buf, 0, -1, false, { "", "  Connecting..." })
  vim.api.nvim_buf_set_option(state.buf, "modifiable", false)

  -- Connect and render
  ws_connect(function()
    vim.schedule(render)
  end)
end

function M.close()
  if state.win and vim.api.nvim_win_is_valid(state.win) then
    vim.api.nvim_win_close(state.win, true)
  end
  state.win = nil
  state.buf = nil
  -- Keep WebSocket connected for background updates
end

function M.disconnect()
  ws_disconnect()
  vim.notify("Procrastinate: Disconnected", vim.log.levels.INFO)
end

function M.refresh()
  if not state.connected then
    ws_connect(function()
      vim.schedule(render)
    end)
  else
    render()
  end
end

function M.toggle_view()
  state.show_completed = not state.show_completed
  render()
end

function M.add_todo()
  vim.ui.input({ prompt = "New todo: " }, function(input)
    if input and input ~= "" then
      local todo = {
        id = math.floor(vim.loop.now()),
        text = input,
        completed = false,
        owner = state.current_user,
        shared_with = {},
      }

      ws_send({ type = "add_todo", todo = todo })
    end
  end)
end

function M.toggle_todo()
  local todo = get_todo_at_cursor()
  if not todo then
    vim.notify("No todo at cursor", vim.log.levels.WARN)
    return
  end

  -- Create a copy with toggled completed status
  local updated_todo = vim.tbl_deep_extend("force", {}, todo)
  updated_todo.completed = not todo.completed

  ws_send({ type = "update_todo", todo = updated_todo })
end

function M.delete_todo()
  local todo = get_todo_at_cursor()
  if not todo then
    vim.notify("No todo at cursor", vim.log.levels.WARN)
    return
  end

  vim.ui.select({ "Yes", "No" }, { prompt = "Delete this todo?" }, function(choice)
    if choice == "Yes" then
      ws_send({ type = "delete_todo", todoId = todo.id })
    end
  end)
end

function M.logout()
  ws_disconnect()
  M.config.session_token = nil
  save_config()
  state.connected = false
  state.current_user = nil
  state.todos = {}
  vim.notify("Procrastinate: Logged out", vim.log.levels.INFO)
end

return M
