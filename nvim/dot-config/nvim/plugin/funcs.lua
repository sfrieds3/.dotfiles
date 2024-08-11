--Open Buildin terminal vertical mode
vim.api.nvim_create_user_command("VT", "vsplit | terminal", { bang = false, nargs = "*" })

--Open Buildin terminal
vim.api.nvim_create_user_command("T", "split | resize 15 | terminal", { bang = true, nargs = "*" })

-- Define a Lua function to create the scratch buffer, execute the shell command, and set the keymap
local function create_scratch_buffer(args)
  -- Create a new scratch buffer
  vim.cmd("new")
  vim.cmd("setlocal buftype=nofile bufhidden=hide noswapfile")

  -- Execute the shell command and capture its output in the buffer
  vim.cmd("r !" .. args)

  -- Get the buffer number of the current (scratch) buffer
  local buf = vim.api.nvim_get_current_buf()

  -- Set the 'q' key to exit the buffer in the scratch buffer
  vim.api.nvim_buf_set_keymap(buf, "n", "q", ":q!<CR>", { noremap = true, silent = true })
end

-- Create a user command 'R' to execute your Lua function, passing along any arguments
vim.api.nvim_create_user_command(
  "R",
  "lua create_scratch_buffer(<q-args>)",
  { bang = false, nargs = "*", complete = "shellcmd" }
)
