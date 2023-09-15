local M = {
  "Vigemus/iron.nvim",
}

function M.config()
  local iron = require("iron.core")

  iron.setup({
    config = {
      scratch_repl = true,
      repl_definition = {
        sh = {
          command = { "fish" },
        },
      },
      -- repl_open_cmd = require("iron.view").right(function()
      --   return math.floor(vim.o.columns * 0.4)
      -- end),
      repl_open_cmd = "vsplit",
    },
    keymaps = {
      send_motion = "\\sc",
      visual_send = "\\sc",
      send_file = "\\sf",
      send_line = "\\sl",
      send_until_cursor = "\\su",
      send_mark = "\\sm",
      mark_motion = "\\mc",
      mark_visual = "\\mc",
      remove_mark = "\\md",
      cr = "\\s<cr>",
      interrupt = "\\s<space>",
      exit = "\\sq",
      clear = "\\cl",
    },
    highlight = {
      italic = true,
    },
    ignore_blank_lines = true,
  })

  vim.keymap.set("n", "\\i", "<Cmd>IronRepl<CR>")
  vim.keymap.set("n", "\\I", "<Cmd>IronReplHere<CR>")
end

return M
