local M = {
  "mfussenegger/nvim-lint",
}

function M.config()
  require("lint").linters_by_ft = {
    -- gitcommit = { "codespell" },
    django = { "djlint" },
    javascript = { "eslint" },
    json = { "jsonlint" },
    -- lua = { "luacheck" },
    markdown = { "markdownlint" },
    sh = { "shellcheck" },
    typescript = { "eslint" },
    yaml = { "yamllint" },
  }

  vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter", "BufLeave" }, {
    group = vim.api.nvim_create_augroup("lint", { clear = true }),
    callback = function()
      require("lint").try_lint()
    end,
  })
end

return M
