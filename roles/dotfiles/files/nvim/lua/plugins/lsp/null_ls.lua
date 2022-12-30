local M = {}

function M.setup()
  require("null-ls").setup({
    sources = {
      -- code actions
      require("null-ls").builtins.code_actions.shellcheck,
      require("null-ls").builtins.code_actions.gitsigns,

      -- diagnostics
      require("null-ls").builtins.diagnostics.flake8,
      require("null-ls").builtins.diagnostics.trail_space.with({
        filetypes = { git = false },
      }),
      require("null-ls").builtins.diagnostics.shellcheck,

      -- formatting
      require("null-ls").builtins.formatting.stylua,
      require("null-ls").builtins.formatting.eslint,
      require("null-ls").builtins.formatting.yapf,
      require("null-ls").builtins.formatting.isort,
      require("null-ls").builtins.formatting.json_tool,
      require("null-ls").builtins.formatting.xmllint,
    },
  })
end

return M
