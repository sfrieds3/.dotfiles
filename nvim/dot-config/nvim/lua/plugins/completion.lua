return {
  {
    "saghen/blink.cmp",
    lazy = false,
    version = "v0.*",
    dependencies = {
      "chrisgrieser/nvim-scissors",
      {
        "saghen/blink.compat",
        opts = {
          -- lazydev.nvim only registers the completion source when nvim-cmp is
          -- loaded, so pretend that we are nvim-cmp, and that nvim-cmp is loaded.
          -- this option only has effect when using lazy.nvim
          -- this should not be required in most cases
          impersontate_nvim_cmp = true,
        },
      },
    },

    opts = {
      keymap = {
        ["<C-space>"] = { "show", "show_documentation", "hide_documentation" },
        ["<C-e>"] = { "hide" },
        ["<Tab>"] = { "select_and_accept", "fallback" },
        ["<C-y>"] = { "select_and_accept", "fallback" },
        ["<C-;>"] = { "select_and_accept" },
        ["<C-p>"] = { "select_prev", "fallback" },
        ["<C-n>"] = { "select_next", "fallback" },

        ["<C-b>"] = { "scroll_documentation_up", "fallback" },
        ["<C-f>"] = { "scroll_documentation_down", "fallback" },

        ["<M-s>"] = { "snippet_forward" },
        ["<M-S-s>"] = { "snippet_backward" },
      },
      highlight = {
        use_nvim_cmp_as_default = true,
      },
      nerd_font_variant = "mono",
      trigger = {
        completion = {
          keyword_range = "full",
        },
        signature_help = { enabled = true },
      },
      windows = {
        ghost_text = {
          enabled = true,
        },
      },
      opts_extend = { "sources.completion.enabled_providers" },
      draw = {
        padding = { 1, 0 },
        columns = { { "label", "label_description", gap = 1 }, { "kind_icon", "kind" } },
        components = {
          kind_icon = { width = { fill = true } },
        },
      },
      -- sources = {
      --   completion = {
      --     enabled_providers = { "lsp", "cody", "lazydev", "path", "snippets", "buffer" },
      --   },
      --   providers = {
      --     lazydev = {
      --       name = "lazydev",
      --       module = "blink.compat.source",
      --     },
      --     cody = {
      --       name = "cody",
      --       module = "blink.compat.source",
      --     },
      --   },
      -- },
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
