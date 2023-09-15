local is_executable = vim.fn.executable

local M = {
  "stevearc/conform.nvim",
}

function M.config()
  require("conform").setup({
    lsp_fallback = true,
    formatters_by_ft = {
      c = { "clang_format" },
      cpp = { "clang_format" },
      fish = { "fish_indent" },
      go = { "gofmt", "goimports" },
      java = { "clang_format" },
      javascript = { "prettier" },
      json = { "jq" },
      python = { "black", "ruff", "yapf" },
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

  vim.keymap.set({ "n", "o", "x", "v" }, "gq", function()
    require("conform").format({ async = true, lsp_fallback = true })
  end)
end

return M
