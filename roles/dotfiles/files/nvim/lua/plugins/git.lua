return {
  { "tpope/vim-fugitive", cmd = "Git", ft = "gitcommit" },
  { "rhysd/git-messenger.vim", cmd = { "GitMessenger" } },
  { "akinsho/git-conflict.nvim", version = "*", config = true },
  {
    "lewis6991/gitsigns.nvim",
    event = "BufReadPre",

    opts = {
      -- signs = {
      --   add = { hl = "GreenSign", text = "+", numhl = "GitSignsAddNr" },
      --   change = { hl = "BlueSign", text = "~", numhl = "GitSignsChangeNr" },
      --   delete = { hl = "RedSign", text = "▁", numhl = "GitSignsDeleteNr" },
      --   topdelete = { hl = "RedSign", text = "▔", numhl = "GitSignsDeleteNr" },
      --   changedelete = { hl = "PurpleSign", text = "~", numhl = "GitSignsChangeNr" },
      --   untracked = { hl = "GreenSign", text = "┆", numhl = "GitSignsAddNr" },
      -- },
      signs = {
        add = { text = "▎" },
        change = { text = "▎" },
        delete = { text = "" },
        topdelete = { text = "" },
        changedelete = { text = "▎" },
        untracked = { text = "▎" },
      },
      signcolumn = true,
      on_attach = function(buffer)
        local gs = package.loaded.gitsigns

        local function map(mode, l, r, desc)
          vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
        end

        -- stylua: ignore start
        map("n", "]h", gs.next_hunk, "Next Hunk")
        map("n", "[h", gs.prev_hunk, "Prev Hunk")
        map({ "n", "v" }, "<leader>ghs", ":Gitsigns stage_hunk<CR>", "Stage Hunk")
        map({ "n", "v" }, "<leader>ghr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
        map("n", "<leader>ghS", gs.stage_buffer, "Stage Buffer")
        map("n", "<leader>ghu", gs.undo_stage_hunk, "Undo Stage Hunk")
        map("n", "<leader>ghR", gs.reset_buffer, "Reset Buffer")
        map("n", "<leader>ghp", gs.preview_hunk, "Preview Hunk")
        map("n", "<leader>ghb", function() gs.blame_line({ full = true }) end, "Blame Line")
        map("n", "<leader>ghd", gs.diffthis, "Diff This")
        map("n", "<leader>ghD", function() gs.diffthis("~") end, "Diff This ~")
        map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
      end,
    },
  },
  {
    "sindrets/diffview.nvim",
    dependencies = "nvim-lua/plenary.nvim",
    cmd = {
      "DiffviewOpen",
      "DiffViewFileHistory",
    },

    opts = {
      keymaps = {
        view = {
          ["q"] = "<cmd>DiffviewClose<cr>",
        },
        file_panel = {
          ["q"] = "<cmd>DiffviewClose<cr>",
        },
        file_history_panel = {
          ["q"] = "<cmd>DiffviewClose<cr>",
        },
        diff1 = {
          ["q"] = "<cmd>DiffviewClose<cr>",
        },
        diff2 = {
          ["q"] = "<cmd>DiffviewClose<cr>",
        },
        diff3 = {
          ["q"] = "<cmd>DiffviewClose<cr>",
        },
        diff4 = {
          ["q"] = "<cmd>DiffviewClose<cr>",
        },
      },
    },

    keys = {
      { "<leader>gvd", "<cmd>DiffviewOpen<cr>", desc = "DiffView Open" },
      { "<leader>gvh", "<cmd>DiffviewFileHistory %<cr>", desc = "DiffView Buffer File History" },
      { "<leader>gvH", "<cmd>DiffviewFileHistory<cr>", desc = "DiffView File History" },
    },
  },

  {
    "NeogitOrg/neogit",
    cmd = { "Neogit" },
    keys = {
      { "_G", "<Cmd>Neogit<CR>" },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
    },

    opts = {
      integrations = { diffview = true },
    },
  },
}
