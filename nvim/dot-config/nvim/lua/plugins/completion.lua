return {
  {
    "saghen/blink.cmp",
    enabled = true,
    event = { "InsertEnter", "CmdLineEnter" },
    version = "v0.*",
    dependencies = {
      "chrisgrieser/nvim-scissors",
    },

    opts = {
      keymap = {
        show = "<C-space>",
        hide = "<C-e>",
        accept = "<Tab>",
        select_prev = { "<Up>", "<C-p>" },
        select_next = { "<Down>", "<C-n>" },

        show_documentation = "<C-space>",
        hide_documentation = "<C-space>",
        scroll_documentation_up = "<C-b>",
        scroll_documentation_down = "<C-f>",

        snippet_forward = "<M-s>",
        snippet_backward = "<M-S-s>",
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
    },
  },
}
