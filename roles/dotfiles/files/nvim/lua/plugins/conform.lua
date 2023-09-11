local is_executable = vim.fn.executable

local M = {
  "stevearc/conform.nvim",
}

function M.config()
  require("conform").setup({
    lsp_fallback = true,
    filetype = {
      c = { "clang_format" },
      cpp = { "clang_format" },
      fish = { "fish_indent" },
      go = { "gofmt", "goimports" },
      java = { "clang_format" },
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
      rust = { "rustfmt" },
      scala = { "scalafmt" },
      typescript = { "prettier" },
      yaml = { "yamlfmt" },
      ["*"] = { "trim_whitespace", "trim_newlines" },
    },
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    callback = function(args)
      require("conform").format({ formatters = { "trim_whitespace", "trim_newlines" }, bufnr = args.bufnr })
    end,
    group = vim.api.nvim_create_augroup("conform:allformat", {}),
  })
end

return M
