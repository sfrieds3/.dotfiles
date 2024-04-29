return {
  { "nvim-treesitter/nvim-treesitter-textobjects", lazy = true },
  { "nvim-treesitter/nvim-treesitter-refactor", lazy = true },
  {
    "mfussenegger/nvim-treehopper",
    keys = {
      { "m", "<cmd><C-U>lua require('tsht').nodes()<cr>", mode = { "o" }, desc = "Treehopper" },
      { "m", "<cmd>lua require('tsht').nodes()<cr>", mode = { "x" }, desc = "Treehopper" },
    },
  },
  {
    "Wansmer/treesj",
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = true,
    -- stylua: ignore
    keys = {
      { "<leader>cm", function() require("treesj").toggle() end, desc = "Treesj Toggle" },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",

    build = ":TSUpdate",

    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },

    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = "all",
        sync_install = false,
        auto_install = true,
        ignore_install = {},
        highlight = {
          enable = true,
          disable = function(lang, buf)
            local max_filesize = 100 * 1024 -- 100 KB
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize and lang ~= "go" then
              return true
            end
          end,
        },
        additional_vim_regex_highlighting = false,
        indent = {
          enable = true,
        },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<C-)>",
            node_incremental = "<C-)>",
            scope_incremental = "<C-s>",
            node_decremental = "<C-()>",
          },
        },
        refactor = {
          highlight_current_scope = { enable = false },
          highlight_definitions = {
            enable = false,
            clear_on_cursor_move = true,
          },
          smart_rename = {
            enable = true,
            keymaps = {
              smart_rename = "<F6>",
            },
          },
          navigation = {
            enable = true,
            keymaps = {
              goto_definition = "gnd",
              list_definitions = "gnD",
              list_definitions_toc = "gO",
              goto_next_usage = "<a-*>",
              goto_previous_usage = "<a-#>",
            },
          },
        },
        matchup = {
          enable = true,
        },
        textobjects = {
          select = {
            enable = true,
            lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              ["aa"] = "@parameter.outer",
              ["ia"] = "@parameter.inner",
              ["am"] = "@function.outer",
              ["im"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = "@class.inner",
              ["al"] = "@loop.outer",
              ["il"] = "@loop.inner",
              ["uc"] = "@comment.outer",
            },
          },
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              ["]m"] = "@function.outer",
              ["]c"] = "@class.outer",
            },
            goto_next_end = {
              ["]M"] = "@function.outer",
              ["]C"] = "@class.outer",
            },
            goto_previous_start = {
              ["[m"] = "@function.outer",
              ["[c"] = "@class.outer",
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
              ["[C"] = "@class.outer",
            },
          },
          swap = {
            enable = true,
            swap_next = {
              ["<leader>a"] = "@parameter.inner",
            },
            swap_previous = {
              ["<leader>A"] = "@parameter.inner",
            },
          },
        },
      })
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    lazy = false,

    keys = {
      {
        "<localleader>T",
        "<cmd>TSContextToggle<cr>",
        desc = "Toggle treesitter-context",
      },
      {
        "[x",
        function()
          require("treesitter-context").go_to_context(vim.v.count1)
        end,
        desc = "Jump to context",
      },
    },

    config = true,
  },
}
