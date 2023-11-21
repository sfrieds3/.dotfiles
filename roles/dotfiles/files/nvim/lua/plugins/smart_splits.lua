return {
  "mrjones2014/smart-splits.nvim",

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
    vim.keymap.set("n", "<A-S-h>", require("smart-splits").resize_left)
    vim.keymap.set("n", "<A-S-j>", require("smart-splits").resize_down)
    vim.keymap.set("n", "<A-S-k>", require("smart-splits").resize_up)
    vim.keymap.set("n", "<A-S-l>", require("smart-splits").resize_right)
    -- moving between splits
    vim.keymap.set("n", "<A-h>", require("smart-splits").move_cursor_left)
    vim.keymap.set("n", "<A-j>", require("smart-splits").move_cursor_down)
    vim.keymap.set("n", "<A-k>", require("smart-splits").move_cursor_up)
    vim.keymap.set("n", "<A-l>", require("smart-splits").move_cursor_right)
    -- swapping buffers between windows
    vim.keymap.set("n", "<leader><leader>h", require("smart-splits").swap_buf_left)
    vim.keymap.set("n", "<leader><leader>j", require("smart-splits").swap_buf_down)
    vim.keymap.set("n", "<leader><leader>k", require("smart-splits").swap_buf_up)
    vim.keymap.set("n", "<leader><leader>l", require("smart-splits").swap_buf_right)
  end,
}
