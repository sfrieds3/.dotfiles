local map = vim.api.nvim_set_keymap
local opts = {noremap = true}

map('n', '\\ev', '<cmd>vsplit $MYVIMRC<cr>', opts)
map('n', '\\es', '<cmd>luafile $MYVIMRC<cr> <cmd>echo "sourced " . $MYVIMRC<cr>', opts)
