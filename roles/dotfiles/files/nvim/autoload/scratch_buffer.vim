" Entry point for :Scratch command
function! scratch_buffer#ScratchBuffer() abort
    new
    setlocal buftype=nofile bufhidden=hide
    let b:undo_ftplugin = "setlocal bt bh<"
endfunction
