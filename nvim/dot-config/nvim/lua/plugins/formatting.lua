return {
  "stevearc/conform.nvim",

  event = { "BufWritePre" },
  cmd = { "ConformInfo", "Format" },

  keys = {
    "gq",
    "<leader>F",
  },

  config = function()
    require("conform").setup({
      format_on_save = function(bufnr)
        if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat or vim.b[bufnr].disable_conform_autoformat then
          return
        end
        return { timeout_ms = 500, lsp_format = "fallback" }
      end,
      formatters_by_ft = {
        c = { "clang_format" },
        clojure = { "joker" },
        cpp = { "clang_format" },
        fish = { "fish_indent" },
        go = { "gofmt", "goimports" },
        java = { "clang_format" },
        jinja = { "djlint" },
        -- json = { "jq" },
        javascript = { "prettierd", "prettier" },
        typescript = { "prettierd", "prettier" },
        javascriptreact = { "prettierd", "prettier" },
        typescriptreact = { "prettierd", "prettier" },
        svelte = { "prettierd", "prettier" },
        css = { "prettierd", "prettier" },
        html = { "prettierd", "prettier" },
        -- yaml = { "prettierd", "prettier" },
        graphql = { "prettierd", "prettier" },
        lua = { "stylua" },
        markdown = { "mdformat" },
        python = { "ruff_format", "ruff_organize_imports" },
        rust = { "rustfmt" },
        scala = { "scalafmt" },
        sh = { "beautysh" },
        toml = { "taplo" },
        yaml = { "yamlfmt" },
        ocaml = { "ocamlformat" },
        ["*"] = { "trim_whitespace", "trim_newlines" },
      },
    })

    vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"

    vim.keymap.set({ "n", "o", "x", "v" }, "gq", function()
      require("conform").format({ lsp_format = "fallback" })
    end, { desc = "Conform Format" })

    vim.api.nvim_create_user_command("FormatDisable", function(args)
      if args.bang then
        -- FormatDisable! will disable formatting just for this buffer
        vim.b.disable_autoformat = true
      else
        vim.g.disable_autoformat = true
      end
    end, {
      desc = "Disable autoformat-on-save",
      bang = true,
    })

    vim.api.nvim_create_user_command("FormatEnable", function()
      vim.b.disable_autoformat = false
      vim.g.disable_autoformat = false
    end, {
      desc = "Re-enable autoformat-on-save",
    })

    vim.api.nvim_create_user_command("Format", function(args)
      local range = nil
      if args.count ~= -1 then
        local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
        range = {
          start = { args.line1, 0 },
          ["end"] = { args.line2, end_line:len() },
        }
      end
      require("conform").format({ async = true, lsp_format = "fallback", range = range })
    end, { range = true })
    vim.keymap.set({ "n", "o", "x", "v" }, "<leader>F", "<cmd>Format<cr>", { desc = "Format" })
  end,
}
