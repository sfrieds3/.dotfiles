return {
  {
    "saghen/blink.cmp",
    enabled = false,
    lazy = false,
    build = "cargo build --release",
    dependencies = {
      "chrisgrieser/nvim-scissors",
      {
        "saghen/blink.compat",
        opts = {
          -- lazydev.nvim only registers the completion source when nvim-cmp is
          -- loaded, so pretend that we are nvim-cmp, and that nvim-cmp is loaded.
          -- this option only has effect when using lazy.nvim
          -- this should not be required in most cases
          impersonate_nvim_cmp = true,
        },
      },
    },

    opts_extend = { "sources.default" },
    opts = {
      keymap = {
        preset = "none",
        ["<M-e>"] = { "show", "show_documentation", "hide_documentation" },
        ["<C-z>"] = { "hide" },
        ["<Tab>"] = { "select_and_accept", "fallback" },
        ["<C-y>"] = { "select_and_accept", "fallback" },
        ["<C-p>"] = { "show", "select_prev", "fallback" },
        ["<C-n>"] = { "show", "select_next", "fallback" },

        ["<C-u>"] = { "scroll_documentation_up", "fallback" },
        ["<C-d>"] = { "scroll_documentation_down", "fallback" },

        ["<M-s>"] = { "snippet_forward" },
        ["<C-l>"] = { "snippet_forward" },
        ["<M-S-s>"] = { "snippet_backward" },
        ["<C-h>"] = { "snippet_backward" },
      },
      completion = {
        keyword = {
          range = "prefix",
        },
        list = {
          selection = {
            preselect = function(ctx)
              return ctx.mode == "cmdline" and "auto_insert" or "preselect"
            end,
          },
        },
        menu = {
          auto_show = function(ctx)
            return ctx.mode == "cmdline"
          end,
          draw = {
            padding = { 1, 0 },
            columns = { { "label", "label_description", gap = 1 }, { "kind_icon", "kind" } },
            components = {
              kind_icon = { width = { fill = true } },
            },
          },
        },
        documentation = {
          auto_show = false,
          auto_show_delay_ms = 500,
        },
        ghost_text = {
          enabled = false,
        },
      },
      signature = {
        enabled = true,
      },
      sources = {
        cmdline = function()
          local type = vim.fn.getcmdtype()
          -- Search forward and backward
          if type == "/" or type == "?" then
            return { "buffer" }
          end
          -- Commands
          if type == ":" then
            return { "cmdline" }
          end
          return {}
        end,
        default = { "lsp", "cody", "path", "snippets", "buffer", "lazydev" },
        providers = {
          lsp = {},
          lazydev = {
            name = "LazyDev",
            module = "lazydev.integrations.blink",
            fallbacks = { "lsp" },
          },
          cody = {
            name = "cody",
            module = "blink.compat.source",
          },
        },
      },
    },
    keys = {
      {
        "<M-.>",
        function()
          if vim.snippet.active() then
            vim.schedule(vim.snippet.stop)
          else
            vim.schedule(require("blink.cmp").hide)
          end
        end,
        mode = { "n", "i" },
        desc = "Hide Completion and/or Stop Snippet",
      },
    },
  },
}
