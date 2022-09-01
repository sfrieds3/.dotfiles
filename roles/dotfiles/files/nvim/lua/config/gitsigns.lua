require("gitsigns").setup({
  signs = {
    add = { hl = "GreenSign", text = "+", numhl = "GitSignsAddNr" },
    change = { hl = "BlueSign", text = "~", numhl = "GitSignsChangeNr" },
    delete = { hl = "RedSign", text = "▁", numhl = "GitSignsDeleteNr" },
    topdelete = { hl = "RedSign", text = "▔", numhl = "GitSignsDeleteNr" },
    changedelete = { hl = "PurpleSign", text = "~", numhl = "GitSignsChangeNr" },
  },
  signcolumn = false,
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
    map({ "n", "v" }, "<leader>hs", ":Gitsigns stage_hunk<CR>")
    map({ "n", "v" }, "<leader>hr", ":Gitsigns reset_hunk<CR>")
    map("n", "<leader>hu", gs.undo_stage_hunk)
    map("n", "<leader>hp", gs.preview_hunk)
    map("n", "<leader>hb", function()
      gs.blame_line({ full = true })
    end)
    map("n", "_hb", gs.toggle_current_line_blame)
    map("n", "<leader>hd", gs.diffthis)
    map("n", "<leader>hD", function()
      gs.diffthis("~")
    end)
    map("n", "<leader>h<Space>", function()
      gs.setloclist(0)
    end)

    -- Text object
    map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")
  end,
})
