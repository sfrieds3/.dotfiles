local M = {
  "nvim-neo-tree/neo-tree.nvim",
  keys = { "<Leader>\\", "<Leader>|", "gd", "<Leader>b", "<Leader>gs" },
  dependencies = {
    "nvim-tree/nvim-web-devicons",
    "MunifTanjim/nui.nvim",
  },
}

function M.config()
  vim.g.neo_tree_remove_legacy_commands = 1
  require("neo-tree").setup({
    opts = {
      filesystem = {
        bind_to_cwd = false,
        follow_current_file = true,
      },
      buffers = {
        follow_current_file = true,
      },
      window = {
        mappings = {
          ["<space>"] = "none",
        },
        fuzzy_finder_mappings = {
          ["<down>"] = "move_cursor_down",
          ["<C-n"] = "move_cursor_down",
          ["<up>"] = "move_cursor_up",
          ["<C-u"] = "move_cursor_up",
        },
      },
    },
  })
  vim.keymap.set("n", "<Leader>\\", "<Cmd>Neotree reveal_force_cwd<CR>")
  vim.keymap.set("n", "<Leader>|", "<Cmd>Neotree reveal<CR>")
  vim.keymap.set("n", "gd", "<Cmd>Neotree float reveal_file=<cfile> reveal_force_cwd<CR>")
  vim.keymap.set("n", "<Leader>b", "<Cmd>Neotree toggle show buffers right<CR>")
  vim.keymap.set("n", "<Leader>gs", "<Cmd>Neotree float git_status<CR>")
end

return M
