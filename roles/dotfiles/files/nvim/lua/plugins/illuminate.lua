local M = {
  "RRethy/vim-illuminate",
}

function M.config()
  require("illuminate").configure({
    providers = { "lsp", "treesitter", "regex" },
    delay = 750,
    filetypes_denylist = {
      "fugitive",
      "NvimTree",
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
