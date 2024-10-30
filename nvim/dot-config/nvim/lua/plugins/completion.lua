return {
  {
    "saghen/blink.cmp",
    lazy = false,
    version = "v0.*",
    dependencies = {
      "chrisgrieser/nvim-scissors",
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
      sources = {
        ghost_text = {
          enabled = true,
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
        mode = "i",
        desc = "Hide Completion and/or Stop Snippet",
      },
    },
  },
}
