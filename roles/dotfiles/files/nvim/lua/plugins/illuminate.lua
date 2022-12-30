local M = {
  "RRethy/vim-illuminate",
}

function M.config()
  require("illuminate").configure({
    providers = { "treesitter", "lsp", "regex" },
    delay = 750,
    filetypes_denylist = {
      "aerial",
      "fugitive",
      "neo-tree",
      "TelescopePrompt",
    },
  })

  vim.keymap.set("n", "<Leader>{", function()
    require("illuminate").next_reference({ wrap = true })
  end)

  vim.keymap.set("n", "<Leader>}", function()
    require("illuminate").next_reference({ reverse = true, wrap = true })
  end)
end

return M
