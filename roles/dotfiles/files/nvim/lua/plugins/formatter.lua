local is_executable = vim.fn.executable

local M = {
  "stevearc/conform.nvim",
}

function M.config()
  require("conform").setup({
    filetype = {
      c = { "clangformat" },
      cpp = { "clangformat" },
      go = { "gofmt", "goimports" },
      javascript = { "prettier" },
      json = { "jq" },
      python = {
        function()
          if is_executable("black") == 1 then
            return { "isort", "black" }
          end
          return {
            "yapf",
            "isort",
          }
        end,
      },
    },
  })
end

return M
