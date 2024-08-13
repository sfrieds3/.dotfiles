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

    -- markdownlint: disable line length warning
    require("lint").linters["markdownlint"].args = {
      "-r",
      "~MD013",
    }

    -- pylint: disable todo warning
    require("lint").linters["pylint"].args = {
      "-f",
      "json",
      "--disable=W0511",
      "--from-stdin",
      function()
        return vim.api.nvim_buf_get_name(0)
      end,
    }

    vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter", "BufLeave" }, {
      group = vim.api.nvim_create_augroup("lint", { clear = true }),
      callback = function()
        require("lint").try_lint()
      end,
    })
  end,
}
