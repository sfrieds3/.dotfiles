local M = {
  "lewis6991/gitsigns.nvim",
  event = "BufReadPre",
}

function M.config()
  require("gitsigns").setup({
    signs = {
      add = { hl = "GreenSign", text = "+", numhl = "GitSignsAddNr" },
      change = { hl = "BlueSign", text = "~", numhl = "GitSignsChangeNr" },
      delete = { hl = "RedSign", text = "▁", numhl = "GitSignsDeleteNr" },
      topdelete = { hl = "RedSign", text = "▔", numhl = "GitSignsDeleteNr" },
      changedelete = { hl = "PurpleSign", text = "~", numhl = "GitSignsChangeNr" },
      untracked = { hl = "GreenSign", text = "┆", numhl = "GitSignsAddNr" },
    },
    signcolumn = true,
    on_attach = function(bufnr)
      local gs = package.loaded.gitsigns

      local function map(mode, l, r, opts)
        opts = opts or {}
        opts.buffer = bufnr
        vim.keymap.set(mode, l, r, opts)
      end

      vim.keymap.set("n", "<Space>G", "<Cmd>Gitsigns toggle_signs<CR>")

      -- Navigation
      map("n", "]c", function()
        if vim.wo.diff then
          return "]c"
        end
        vim.schedule(function()
          gs.next_hunk({ wrap = false })
        end)
        return "<Ignore>"
      end, { expr = true })

      map("n", "[c", function()
        if vim.wo.diff then
          return "[c"
        end
        vim.schedule(function()
          gs.prev_hunk({ wrap = false })
        end)
        return "<Ignore>"
      end, { expr = true })

      -- Actions
      map({ "n", "v" }, "<leader>hs", gs.stage_hunk, { desc = "Gitsigns: [h]unk [s]tage" })
      map({ "n", "v" }, "<leader>hr", gs.reset_hunk, { desc = "Gitsigns: [h]unk [r]eset" })
      map("n", "<Leader>hu", gs.undo_stage_hunk, { desc = "Gitsigns: [h]unk [u]ndo stage" })
      map("n", "<Leader>hv", gs.preview_hunk, { desc = "Gitsigns: [h]unk [v]iew" })
      map("n", "<Leader>hB", function()
        gs.blame_line({ full = true })
      end, { desc = "Gitsigns: [h]unk [b]lame" })
      map("n", "<Leader>hb", gs.toggle_current_line_blame, { desc = "Gitsigns: toggle current line [b]lame" })
      map("n", "<Leader>hd", gs.diffthis, { desc = "Gitsigns: [h]unk [d]iffthis" })
      map("n", "<Leader>hD", function()
        gs.diffthis("~")
      end, { desc = "Gitsigns: [h]unk [D]iffthis(~)" })
      map("n", "<leader>hl", function()
        gs.setloclist(0)
      end, { desc = "Gitsigns: [h]unk set [l]oclist" })

      -- Text object
      map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")
    end,
  })
end

return M
