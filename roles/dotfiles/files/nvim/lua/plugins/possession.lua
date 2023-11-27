local M = {
  "jedrzejboczar/possession.nvim",
}

function M.config()
  require("possession").setup({
    autosave = {
      current = true,
      tmp = false,
    },
    plugins = {
      close_windows = {
        hooks = { "before_save", "before_load" },
        preserve_layout = true, -- or fun(win): boolean
        match = {
          floating = true,
          buftype = {},
          filetype = {},
          custom = false, -- or fun(win): boolean
        },
      },
      delete_hidden_buffers = false,
      nvim_tree = true,
      neo_tree = true,
      symbols_outline = true,
      tabby = true,
      dap = true,
      dapui = true,
      delete_buffers = false,
    },
    commands = {
      save = "SSave",
      load = "SLoad",
      delete = "SDelete",
      list = "SList",
      rename = "SRename",
      close = "SClose",
    },
  })

  vim.keymap.set(
    "n",
    "\\S",
    require("telescope").extensions.possession.list,
    { desc = "[S]essionManager: load session" }
  )
end

return M
