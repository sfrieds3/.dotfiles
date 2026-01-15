return {
  "mfussenegger/nvim-lint",

  event = { "BufWritePre" },

  config = function()
    require("lint").linters_by_ft = {
      -- gitcommit = { "codespell" },
      ["yaml.ansible"] = { "ansible_lint" },
      dockerfile = { "hadolint" },
      django = { "djlint" },
      java = { "checkstyle" },
      json = { "jsonlint" },
      rst = { "rstcheck" },
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

    -- checkstyle: use global config
    local config_home = vim.env.XDG_CONFIG_HOME or vim.fn.expand("~/.config")
    require("lint").linters.checkstyle.args = {
      "-c",
      config_home .. "/checkstyle/checkstyle.xml",
    }

    vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter", "BufLeave" }, {
      group = vim.api.nvim_create_augroup("lint", { clear = true }),
      callback = function()
        require("lint").try_lint()
      end,
    })
  end,
}
