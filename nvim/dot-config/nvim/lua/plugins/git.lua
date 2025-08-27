return {
  {
    "tpope/vim-fugitive",
    cmd = "Git",
    ft = "gitcommit",
    keys = {
      { "<leader>gl", "<cmd>Git log %<cr>", desc = "Git Log File" },
      { "<leader>gL", "<cmd>Git log<cr>", desc = "Git Log Repo" },
      { "<leader>gdd", "<cmd>Git diff<cr>", desc = "Git Diff" },
      { "<leader>gdc", "<cmd>Git diff --cached<cr>", desc = "Git Diff --cached" },
    },
  },
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

        local nav_hunk_config = {
          wrap = true,
          navigation_message = true,
          preview = true,
        }

        local nav_hunk_config_no_preview = {
          wrap = true,
          navigation_message = true,
          preview = false,
        }

        local target_all = { target = "all" }
        local nav_hunk_all_config = vim.tbl_deep_extend("keep", target_all, nav_hunk_config)
        local nav_hunk_all_config_no_preview = vim.tbl_deep_extend("keep", target_all, nav_hunk_config_no_preview)

        -- stylua: ignore start
        map("n", "<leader>gb", gs.blame, "Gitsigns Blame")
        map("n", "]h", function() gs.nav_hunk("next", nav_hunk_config_no_preview) end, "Next Unstaged Hunk")
        map("n", "<leader>]h", function() gs.nav_hunk("next", nav_hunk_all_config_no_preview) end, "Next Hunk")
        map("n", "<leader>]H", function() gs.nav_hunk("next", nav_hunk_all_config) end, "Next Hunk")
        map("n", "[h", function() gs.nav_hunk("prev", nav_hunk_config_no_preview) end, "Prev Unstaged Hunk")
        map("n", "<leader>[h", function() gs.nav_hunk("prev", nav_hunk_all_config_no_preview) end, "Prev Hunk")
        map("n", "<leader>[H", function() gs.nav_hunk("prev", nav_hunk_all_config) end, "Prev Hunk")
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
      "DiffviewFileHistory",
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
      { "<leader>gvu", "<cmd>DiffviewOpen @ @{upstream}<cr>", desc = "DiffView @ @{upstream}" },
      { "<leader>gvm", "<cmd>DiffviewOpen master..HEAD<cr>", desc = "DiffView master..HEAD" },
      { "<leader>gvM", "<cmd>DiffviewOpen main..HEAD<cr>", desc = "DiffView main..HEAD" },
      { "<leader>gvh", "<cmd>DiffviewFileHistory %<cr>", desc = "DiffView Buffer File History" },
      { "<leader>gvH", "<cmd>DiffviewFileHistory<cr>", desc = "DiffView File History" },
    },
  },
}
