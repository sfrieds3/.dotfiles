return {
  "stevearc/conform.nvim",

  event = { "BufWritePre" },
  cmd = { "ConformInfo" },

  keys = {
    "gq",
  },

  config = function()
    require("conform").setup({
      lsp_fallback = true,
      formatters_by_ft = {
        c = { "clang_format" },
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
        -- markdown = { "prettierd", "prettier" },
        graphql = { "prettierd", "prettier" },
        lua = { "stylua" },
        markdown = { "mdformat" },
        python = { "ruff_format", "isort" },
        rust = { "rustfmt" },
        scala = { "scalafmt" },
        sh = { "shellcheck" },
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
      require("conform").format({ async = true, lsp_fallback = true })
    end)
  end,
}
