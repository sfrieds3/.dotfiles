local M = {}
local is_executable = vim.fn.executable

function M.setup()
  require("null-ls").setup({
    sources = {
      -- code actions
      require("null-ls").builtins.code_actions.gitsigns,
      require("null-ls").builtins.code_actions.shellcheck,

      -- diagnostics
      -- require("null-ls").builtins.diagnostics.flake8,
      require("null-ls").builtins.diagnostics.djlint,
      require("null-ls").builtins.diagnostics.pylint.with({
        condition = function(_)
          return is_executable("pylint") == 1
        end,
      }),
      require("null-ls").builtins.diagnostics.ruff,
      require("null-ls").builtins.diagnostics.shellcheck,
      require("null-ls").builtins.diagnostics.trail_space.with({
        filetypes = { git = false },
      }),

      -- formatting
      require("null-ls").builtins.formatting.eslint,
      require("null-ls").builtins.formatting.json_tool,
      -- require("null-ls").builtins.formatting.pyink,
      require("null-ls").builtins.formatting.reorder_python_imports,
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
