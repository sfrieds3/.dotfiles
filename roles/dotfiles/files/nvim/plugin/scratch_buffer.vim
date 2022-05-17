"
" scratch_buffer.vim: User command to open a scratch buffer.
"
" Author: Tom Ryder <tom@sanctum.geek.nz>
" License: Same as Vim itself
"
if exists('loaded_scratch_buffer') || &compatible || v:version < 700
  finish
endif
let loaded_scratch_buffer = 1

" Command to open scratch buffer
command! Scratch
      \ call scratch_buffer#ScratchBuffer()
