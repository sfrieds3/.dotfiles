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

vim.keymap.set("n", "<C-{>", function()
  require("illuminate").next_reference({ wrap = true })
end)

vim.keymap.set("n", "<C-}>", function()
  require("illuminate").next_reference({ reverse = true, wrap = true })
end)
