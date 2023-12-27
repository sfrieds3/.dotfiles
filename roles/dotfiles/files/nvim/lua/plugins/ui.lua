return {
  {
    "folke/which-key.nvim",
    plugins = { spelling = true },
  },
  "rcarriga/nvim-notify",
  {
    "luukvbaal/statuscol.nvim",
    opts = { setopt = true },
  },
  {
    "utilyre/barbecue.nvim",
    name = "barbecue",
    version = "*",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons",
    },
    opts = {},
  },
  {
    "Wansmer/symbol-usage.nvim",
    enabled = false,
    event = "LspAttach",
    config = function()
      require("symbol-usage").setup({
        vt_position = "end_of_line",
      })
    end,
  },
  {
    "j-hui/fidget.nvim",
    dependencies = "neovim/nvim-lspconfig",
    event = "LspAttach",
    config = function()
      require("fidget").setup({
        progress = {
          suppress_on_insert = true,
          ignore_done_already = true,
          display = {
            render_limit = 3,
          },
        },
      })
    end,
  },
  {
    "andymass/vim-matchup",
    config = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_matchparen_deferred_show_delay = 500
      vim.g.matchup_matchparen_deferred_hide_delay = 500
      vim.g.matchup_matchparen_timeout = 100
      vim.g.matchup_matchparen_deferred = 1
    end,
  },
  {
    "hedyhli/outline.nvim",
    opts = {
      outline_items = {
        highlight_hovered_item = true,
        show_symbol_details = true,
        show_symbol_lineno = false,
      },
      outline_window = {
        show_cursorline = true,
        hide_cursor = true,
      },
      symbol_folding = {
        autofold_depth = 1,
      },
      winblend = 10,
      keymaps = {
        hover_symbol = "<leader>e",
      },
    },
    cmd = { "Outline", "OutlineOpen" },
    keys = {
      { "<Leader><CR>", "<cmd>Outline<cr>", desc = "Outline: toggle" },
    },
  },
  {
    "s1n7ax/nvim-window-picker",
    version = "v2.*",
    config = function()
      require("window-picker").setup({
        hint = "floating-big-letter",
      })

      vim.keymap.set("n", "<Leader>w", function()
        local winid = require("window-picker"):pick_window() or vim.api.nvim_get_current_win()
        vim.api.nvim_set_current_win(winid)
      end, { desc = "Pick a window" })
    end,
  },
  {
    "RRethy/vim-illuminate",

    opts = {
      providers = { "lsp", "treesitter", "regex" },
      delay = 200,
      large_file_cutff = 2000,
      large_file_override = {
        providers = { "lsp" },
      },
      filetypes_denylist = {
        "fugitive",
        "NvimTree",
        "TelescopePrompt",
      },
    },

    keys = {
      {
        "[[",
        function()
          require("illuminate").next_reference({ reverse = true, wrap = true })
        end,
        desc = "Previous Reference",
      },
      {
        "]]",
        function()
          require("illuminate").next_reference({ wrap = true })
        end,
        desc = "Next Reference",
      },
    },
  },
  {
    "akinsho/bufferline.nvim",
    version = "*",
    opts = {
      options = {
        diagnostics = "nvim_lsp",
        mode = "tabs",
      },
    },
  },
}
