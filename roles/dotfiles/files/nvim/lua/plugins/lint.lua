return {
  "mfussenegger/nvim-lint",

  event = { "BufWritePre" },

  config = function()
    require("lint").linters_by_ft = {
      -- gitcommit = { "codespell" },
      dockerfile = { "hadolint" },
      django = { "djlint" },
      javascript = { "eslint_d" },
      typescript = { "eslint_d" },
      javascriptreact = { "eslint_d" },
      typescriptreact = { "eslint_d" },
      svelte = { "eslint_d" },
      json = { "jsonlint" },
      -- lua = { "luacheck" },
      markdown = { "markdownlint" },
      sh = { "shellcheck" },
      yaml = { "yamllint" },
    }

    require("lint").linters["markdownlint"].args = {
      "-r",
      "~MD013",
    }

    require("lint").linters["pylint"].args = {
      "-f",
      "json",
      "--disable=W0511",
    }

    vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter", "BufLeave" }, {
      group = vim.api.nvim_create_augroup("lint", { clear = true }),
      callback = function()
        require("lint").try_lint()
      end,
    })
  end,
}
