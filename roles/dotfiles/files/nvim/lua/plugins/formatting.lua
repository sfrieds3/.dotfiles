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
      formatters_by_ft = {
        c = { "clang_format" },
        clojure = { "joker" },
        cpp = { "clang_format" },
        fish = { "fish_indent" },
        go = { "gofmt", "goimports" },
        java = { "clang_format" },
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
        yaml = { "yamlfmt" },
        ocaml = { "ocamlformat" },
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
      require("conform").format({ lsp_format = "fallback" })
    end, { desc = "Conform Format" })

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
