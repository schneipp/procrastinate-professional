-- procrastinate.nvim
-- Neovim client for Procrastinate-Professional

local M = {}

-- Configuration
M.config = {
  server_url = "ws://localhost:8090/ws",
  session_token = nil,
  width = 0.8,
  height = 0.8,
  border = "rounded",
}

-- State
local state = {
  todos = {},
  users = {},
  current_user = nil,
  connected = false,
  job_id = nil,
  buf = nil,
  win = nil,
  show_completed = false,
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
end

-- Strip HTML tags
local function strip_html(str)
  if not str then return "" end
  return str:gsub("<[^>]+>", "")
end

-- Get todo at cursor
local function get_todo_at_cursor()
  local line = vim.api.nvim_win_get_cursor(state.win)[1]
  local todo_index = vim.b[state.buf].todo_lines and vim.b[state.buf].todo_lines[line]
  if todo_index then
    return state.todos[todo_index]
  end
  return nil
end

-- Send message via curl (simpler than websocket)
local function send_message(msg)
  -- We'll use HTTP polling approach for simplicity
  -- For real WebSocket, you'd need a plugin like nvim-websocket
  vim.notify("Procrastinate: Sending...", vim.log.levels.INFO)
end

-- Fetch todos via HTTP
local function fetch_todos(callback)
  if not M.config.session_token then
    vim.notify("Procrastinate: No session token configured", vim.log.levels.ERROR)
    return
  end

  local url = M.config.server_url:gsub("ws://", "http://"):gsub("wss://", "https://"):gsub("/ws", "")

  -- Use curl to fetch initial state by connecting briefly
  local cmd = string.format(
    'curl -s -N -H "Cookie: session_token=%s" --max-time 3 "%s/ws" 2>/dev/null | head -1',
    M.config.session_token,
    url
  )

  vim.fn.jobstart(cmd, {
    stdout_buffered = true,
    on_stdout = function(_, data)
      if data and data[1] and data[1] ~= "" then
        local ok, json = pcall(vim.json.decode, data[1])
        if ok and json.type == "todo_list" then
          state.todos = json.todos or {}
          state.users = json.users or {}
          state.current_user = json.username
          state.connected = true
          if callback then callback() end
        end
      end
    end,
    on_stderr = function(_, data)
      if data and data[1] and data[1] ~= "" then
        vim.notify("Procrastinate: " .. data[1], vim.log.levels.ERROR)
      end
    end,
  })
end

-- HTTP request helper
local function http_request(method, endpoint, body, callback)
  if not M.config.session_token then
    vim.notify("Procrastinate: No session token configured", vim.log.levels.ERROR)
    return
  end

  local url = M.config.server_url:gsub("ws://", "http://"):gsub("wss://", "https://"):gsub("/ws", "")

  local cmd
  if body then
    local json_body = vim.json.encode(body)
    cmd = string.format(
      'curl -s -X POST -H "Cookie: session_token=%s" -H "Content-Type: application/json" -d \'%s\' "%s%s"',
      M.config.session_token,
      json_body:gsub("'", "'\\''"),
      url,
      endpoint
    )
  else
    cmd = string.format(
      'curl -s -H "Cookie: session_token=%s" "%s%s"',
      M.config.session_token,
      url,
      endpoint
    )
  end

  vim.fn.jobstart(cmd, {
    stdout_buffered = true,
    on_stdout = function(_, data)
      if callback and data then
        callback(data[1])
      end
    end,
  })
end

-- WebSocket-style operation via curl
local function ws_send(msg, callback)
  if not M.config.session_token then
    vim.notify("Procrastinate: No session token configured", vim.log.levels.ERROR)
    return
  end

  local url = M.config.server_url:gsub("ws://", "http://"):gsub("wss://", "https://"):gsub("/ws", "")
  local json_msg = vim.json.encode(msg)

  -- Use websocat if available, otherwise fall back to curl with upgrade
  local cmd = string.format(
    'echo \'%s\' | timeout 2 websocat -H "Cookie: session_token=%s" "%s" 2>/dev/null | head -2',
    json_msg:gsub("'", "'\\''"),
    M.config.session_token,
    M.config.server_url
  )

  vim.fn.jobstart(cmd, {
    stdout_buffered = true,
    on_stdout = function(_, data)
      if data then
        for _, line in ipairs(data) do
          if line and line ~= "" then
            local ok, json = pcall(vim.json.decode, line)
            if ok then
              if json.type == "todo_list" then
                state.todos = json.todos or {}
              elseif json.type == "todo_added" or json.type == "todo_updated" then
                -- Refresh
                M.refresh()
              elseif json.type == "todo_deleted" then
                M.refresh()
              end
            end
          end
        end
      end
      if callback then callback() end
    end,
    on_exit = function()
      if callback then callback() end
    end,
  })
end

-- Render the todo list
local function render()
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
  local status = state.connected and "  Connected as: " .. (state.current_user or "unknown") or "  Disconnected"
  table.insert(lines, status)
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
      todo_lines[line_num] = i

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

      -- Store todo reference
      local actual_index
      for idx, t in ipairs(state.todos) do
        if t.id == todo.id then
          actual_index = idx
          break
        end
      end
      todo_lines[line_num] = actual_index
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

-- Public functions

function M.setup(opts)
  M.config = vim.tbl_deep_extend("force", M.config, opts or {})
  setup_highlights()

  -- Set up the keybinding
  vim.keymap.set("n", "<leader>nrn", M.open, { desc = "Procrastinate: Open todo list" })
end

function M.open()
  if state.win and vim.api.nvim_win_is_valid(state.win) then
    vim.api.nvim_set_current_win(state.win)
    return
  end

  create_window()

  -- Show loading
  vim.api.nvim_buf_set_option(state.buf, "modifiable", true)
  vim.api.nvim_buf_set_lines(state.buf, 0, -1, false, { "", "  Loading..." })
  vim.api.nvim_buf_set_option(state.buf, "modifiable", false)

  -- Fetch and render
  fetch_todos(function()
    vim.schedule(render)
  end)
end

function M.close()
  if state.win and vim.api.nvim_win_is_valid(state.win) then
    vim.api.nvim_win_close(state.win, true)
  end
  state.win = nil
  state.buf = nil
end

function M.refresh()
  fetch_todos(function()
    vim.schedule(render)
  end)
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

      ws_send({ type = "add_todo", todo = todo }, function()
        vim.schedule(function()
          M.refresh()
        end)
      end)
    end
  end)
end

function M.toggle_todo()
  local todo = get_todo_at_cursor()
  if not todo then
    vim.notify("No todo at cursor", vim.log.levels.WARN)
    return
  end

  todo.completed = not todo.completed

  ws_send({ type = "update_todo", todo = todo }, function()
    vim.schedule(function()
      M.refresh()
    end)
  end)
end

function M.delete_todo()
  local todo = get_todo_at_cursor()
  if not todo then
    vim.notify("No todo at cursor", vim.log.levels.WARN)
    return
  end

  vim.ui.select({ "Yes", "No" }, { prompt = "Delete this todo?" }, function(choice)
    if choice == "Yes" then
      ws_send({ type = "delete_todo", todoId = todo.id }, function()
        vim.schedule(function()
          M.refresh()
        end)
      end)
    end
  end)
end

return M
