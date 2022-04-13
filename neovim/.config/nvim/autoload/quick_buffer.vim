if exists('g:loaded_quick_buffer')
    finish
endif
let g:loaded_quick_buffer = 1

function! quick_buffer#QuickBuffer(pattern) abort
  if empty(a:pattern)
    call feedkeys(":B \<C-d>")
    return
  elseif a:pattern is '*'
    call feedkeys(":ls!\<cr>:B ")
    return
  elseif a:pattern =~ '^\d\+$'
    execute 'buffer' a:pattern
    return
  endif
  let l:globbed = '*' . join(split(a:pattern, ' '), '*') . '*'
  try
    execute 'buffer' l:globbed
  catch
    call feedkeys(':B ' . l:globbed . "\<C-d>\<C-u>B " . a:pattern)
  endtry
endfun

