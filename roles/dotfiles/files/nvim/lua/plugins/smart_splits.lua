return {
  "mrjones2014/smart-splits.nvim",
  -- build = "./kitty/install-kittens.bash",

  config = function()
    require("smart-splits").setup({
      -- Ignored filetypes (only while resizing)
      ignored_filetypes = {
        "nofile",
        "quickfix",
        "prompt",
      },
      -- Ignored buffer types (only while resizing)
      ignored_buftypes = { "neo-tree" },
      resize_mode = {
        -- key to exit persistent resize mode
        quit_key = "<ESC>",
        -- keys to use for moving in resize mode
        -- in order of left, down, up' right
        resize_keys = { "h", "j", "k", "l" },
        -- set to true to silence the notifications
        -- when entering/exiting persistent resize mode
        silent = false,
        -- must be functions, they will be executed when
        -- entering or exiting the resize mode
        hooks = {
          on_enter = nil,
          on_leave = nil,
        },
        multiplexer_integration = nil,
      },
    })
    vim.keymap.set("n", "<A-S-h>", require("smart-splits").resize_left, { desc = "smart-split resize left" })
    vim.keymap.set("n", "<A-S-j>", require("smart-splits").resize_down, { desc = "smart-split resize down" })
    vim.keymap.set("n", "<A-S-k>", require("smart-splits").resize_up, { desc = "smart-split resize up" })
    vim.keymap.set("n", "<A-S-l>", require("smart-splits").resize_right, { desc = "smart-split resize right" })
    -- moving between splits
    vim.keymap.set("n", "<A-h>", require("smart-splits").move_cursor_left, { desc = "smart-split move cursor left" })
    vim.keymap.set("n", "<A-j>", require("smart-splits").move_cursor_down, { desc = "smart-split move cursor down" })
    vim.keymap.set("n", "<A-k>", require("smart-splits").move_cursor_up, { desc = "smart-split move cursor up" })
    vim.keymap.set("n", "<A-l>", require("smart-splits").move_cursor_right, { desc = "smart-split move cursor right" })
    -- swapping buffers between windows
    vim.keymap.set("n", "<leader>wh", require("smart-splits").swap_buf_left, { desc = "smart-split swap buf left" })
    vim.keymap.set("n", "<leader>wj", require("smart-splits").swap_buf_down, { desc = "smart-split swap buf down" })
    vim.keymap.set("n", "<leader>wk", require("smart-splits").swap_buf_up, { desc = "smart-split swap buf up" })
    vim.keymap.set("n", "<leader>wl", require("smart-splits").swap_buf_right, { desc = "smart-split swap buf right" })
  end,
}
