local is_executable = vim.fn.executable

local M = {
  "mhartington/formatter.nvim",
}

function M.config()
  require("formatter").setup({
    filetype = {
      c = {
        require("formatter.filetypes.c").clangformat,
      },
      cpp = {
        require("formatter.filetypes.cpp").clangformat,
      },
      go = {
        require("formatter.filetypes.go").gofmt,
        require("formatter.filetypes.go").goimports,
      },
      javascript = {
        require("formatter.filetypes.javascript").prettier,
      },
      json = {
        require("formatter.filetypes.json").jq,
      },
      lua = {
        require("formatter.filetypes.lua").stylua,
      },
      python = {
        function()
          if is_executable("black") == 1 then
            return { require("formatter.filetypes.python").black }
          end
          return {
            require("formatter.filetypes.python").yapf,
            require("formatter.filetypes.python").isort,
          }
        end,
      },
      ["*"] = {
        require("formatter.filetypes.any").remove_trailing_whitespace,
      },
    },
  })
end

return M
