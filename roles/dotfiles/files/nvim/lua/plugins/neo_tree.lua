local M = {
  "nvim-neo-tree/neo-tree.nvim",
  keys = "<Leader>\\",
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
      window = {
        mappings = {
          ["<space>"] = "none",
        },
      },
    },
  })
  vim.keymap.set("n", "<Leader>\\", "<Cmd>Neotree<CR>")
  vim.keymap.set("n", "_F", "<Cmd>Neotree reveal<CR>")
end

return M
