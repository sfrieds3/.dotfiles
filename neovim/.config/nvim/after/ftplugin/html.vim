setlocal shiftwidth=2 softtabstop=2

iabbrev <buffer> </ </<C-X><C-O>
imap <buffer> <C-Space> <C-X><C-O>

let b:undo_ftplugin = "setlocal sw< sts<"
