vim.g.undotree_WindowLayout = 2
vim.keymap.set("n", "_U", "<Cmd>exec('UndotreeToggle <bar> UndotreeFocus')<CR>")
vim.keymap.set("n", "<Leader>u", "<Cmd>exec('UndotreeFocus')<CR>")

vim.cmd([[
function! g:Undotree_CustomMap()
    nmap <buffer> K <plug>UndotreeNextState
    nmap <buffer> J <plug>UndotreePreviousState
    nmap <buffer> \u q
endfunction
]])
