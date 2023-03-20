local M = {}

function M.setup()
  require("null-ls").setup({
    sources = {
      -- code actions
      require("null-ls").builtins.code_actions.gitsigns,
      require("null-ls").builtins.code_actions.shellcheck,

      --} d}iagnostics
      -- require("null-ls").builtins.diagnostics.flake8,
      require("null-ls").builtins.diagnostics.pylint,
      require("null-ls").builtins.diagnostics.ruff,
      require("null-ls").builtins.diagnostics.shellcheck,
      require("null-ls").builtins.diagnostics.trail_space.with({
        filetypes = { git = false },
      }),

      -- formatting
      require("null-ls").builtins.formatting.eslint,
      -- require("null-ls").builtins.formatting.isort,
      require("null-ls").builtins.formatting.json_tool,
      require("null-ls").builtins.formatting.ruff,
      require("null-ls").builtins.formatting.stylua,
      require("null-ls").builtins.formatting.trim_whitespace,
      require("null-ls").builtins.formatting.trim_newlines,
      require("null-ls").builtins.formatting.xmllint,
      require("null-ls").builtins.formatting.yapf,
    },
  })
end

return M
