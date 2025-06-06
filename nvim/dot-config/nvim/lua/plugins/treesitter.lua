return {
  { "nvim-treesitter/nvim-treesitter-textobjects", lazy = true },
  {
    "Wansmer/treesj",
    dependencies = "nvim-treesitter/nvim-treesitter",
    -- stylua: ignore
    keys = {
      { "<leader>cj", function() require("treesj").toggle() end, desc = "Treesj Toggle" },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",

    build = ":TSUpdate",

    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    opts = {
      ensure_installed = "all",
      sync_install = false,
      auto_install = true,
      ignore_install = {},
      highlight = {
        enable = true,
        -- disable = function(lang, buf)
        --   local max_filesize = 100 * 1024 -- 100 KB
        --   local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
        --   if ok and stats and stats.size > max_filesize and lang ~= "go" then
        --     return true
        --   end
        -- end,
      },
      additional_vim_regex_highlighting = false,
      indent = {
        enable = true,
      },
      matchup = {
        enable = true,
      },
      textobjects = {
        select = {
          enable = true,
          lookahead = true,
          keymaps = {
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
          set_jumps = true,
          goto_next_start = {
            -- ["]f"] = "@function.outer",
            -- ["]c"] = "@class.outer",
            ["]m"] = "@function.outer",
          },
          goto_next_end = {
            -- ["]F"] = "@function.outer",
            -- ["]C"] = "@class.outer",
            ["]M"] = "@function.outer",
          },
          goto_previous_start = {
            -- ["[f"] = "@function.outer",
            -- ["[c"] = "@class.outer",
            ["[m"] = "@function.outer",
          },
          goto_previous_end = {
            -- ["[F"] = "@function.outer",
            -- ["[C"] = "@class.outer",
            ["[M"] = "@function.outer",
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
    },
    config = function(_, opts)
      require("nvim-treesitter.configs").setup(opts)
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    lazy = false,

    keys = {
      {
        "<M-t>",
        function()
          require("treesitter-context").toggle()
        end,
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
    opts = {
      max_lines = 10,
      trim_scope = "inner",
    },
  },
}
