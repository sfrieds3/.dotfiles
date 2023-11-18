return {
  "rcarriga/nvim-notify",
  "romainl/vim-qlist",
  { "RRethy/nvim-align", cmd = { "Align" } },
  { "romainl/vim-qf", ft = { "qf" } },
  { "chrisbra/NrrwRgn", cmd = { "NR", "NarrowRegion" } },
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
    opts = {
      -- configurations go here
    },
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
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    config = function()
      require("ibl").setup({
        enabled = false,
        indent = { smart_indent_cap = true },
        scope = { show_start = false, show_end = false, highlight = { "Whitespace" } },
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
    "SmiteshP/nvim-navic",
    event = "VeryLazy",
    config = function()
      vim.g.navic_silence = true
      require("nvim-navic").setup({ separator = " ", highlight = true, depth_limit = 5 })
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

    config = function()
      require("illuminate").configure({
        providers = { "lsp", "treesitter", "regex" },
        delay = 750,
        filetypes_denylist = {
          "fugitive",
          "NvimTree",
          "TelescopePrompt",
        },
      })

      vim.keymap.set("n", "<Leader>{", function()
        require("illuminate").next_reference({ wrap = true })
      end)

      vim.keymap.set("n", "<Leader>}", function()
        require("illuminate").next_reference({ reverse = true, wrap = true })
      end)
    end,
  },
  {
    "akinsho/bufferline.nvim",
    version = "*",
    config = function()
      require("bufferline").setup({
        options = {
          diagnostics = "nvim_lsp",
          mode = "tabs",
        },
      })
    end,
  },
}
