local is_executable = vim.fn.executable

return {
  {
    "numToStr/Comment.nvim",
    config = true,
  },
  {
    "stevearc/conform.nvim",
    config = function()
      require("conform").setup({
        lsp_fallback = true,
        formatters_by_ft = {
          c = { "clang_format" },
          cpp = { "clang_format" },
          fish = { "fish_indent" },
          go = { "gofmt", "goimports" },
          java = { "clang_format" },
          javascript = { "prettier" },
          -- json = { "jq" },
          lua = { "stylua" },
          markdown = { "mdformat" },
          python = { "ruff_format", "black" },
          rust = { "rustfmt" },
          scala = { "scalafmt" },
          sh = { "shellcheck" },
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
    end,
  },
  {
    "mfussenegger/nvim-lint",
    config = function()
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
    end,
  },
  {
    "Vigemus/iron.nvim",
    config = function()
      local iron = require("iron.core")

      iron.setup({
        config = {
          scratch_repl = true,
          repl_definition = {
            sh = {
              command = { "fish" },
            },
          },
          -- repl_open_cmd = require("iron.view").right(function()
          --   return math.floor(vim.o.columns * 0.4)
          -- end),
          repl_open_cmd = "vsplit",
        },
        keymaps = {
          send_motion = "\\sc",
          visual_send = "\\sc",
          send_file = "\\sf",
          send_line = "\\sl",
          send_until_cursor = "\\su",
          send_mark = "\\sm",
          mark_motion = "\\mc",
          mark_visual = "\\mc",
          remove_mark = "\\md",
          cr = "\\s<cr>",
          interrupt = "\\s<space>",
          exit = "\\sq",
          clear = "\\cl",
        },
        highlight = {
          italic = true,
        },
        ignore_blank_lines = true,
      })

      vim.keymap.set("n", "\\i", "<Cmd>IronRepl<CR>")
      vim.keymap.set("n", "\\I", "<Cmd>IronReplHere<CR>")
    end,
  },
}
