local bufnr = vim.api.nvim_get_current_buf()

vim.bo.makeprg = "cargo build"
-- vim.bo.compiler = "cargo"

vim.api.nvim_create_user_command("CargoTest", function(params)
  -- Insert args at the '$*' in the grepprg
  local task = require("overseer").new_task({
    cmd = vim.cmd([[!cargo test ]]),
    components = {
      {
        "on_output_summarize",
      },
      { "on_complete_dispose", timeout = 30 },
      "default",
    },
  })
  task:start()
end, { nargs = "*", bang = true, complete = "file" })
