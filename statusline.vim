" statusline plugin


function! InsertStatuslineColor(mode)
  if a:mode == 'i'
    hi statusline guibg=Cyan ctermfg=6 guifg=Black ctermbg=0
  elseif a:mode == 'r'
    hi statusline guibg=Purple ctermfg=5 guifg=Black ctermbg=0
  else
    hi statusline guibg=DarkRed ctermfg=1 guifg=Black ctermbg=0
  endif
endfunction

augroup statusline
  autocmd!
  autocmd InsertEnter * call InsertStatuslineColor(v:insertmode)
  autocmd InsertLeave * hi statusline guibg=DarkGrey ctermfg=8 guifg=black ctermbg=15
augroup END

" default the statusline to dark grey when entering Vim
hi statusline guibg=DarkGrey ctermfg=8 guifg=black ctermbg=15

function! LinterStatus() abort
  let l:counts = ale#statusline#Count(bufnr(''))

  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors

  return l:counts.total == 0 ? ' OK' : printf(
        \   '%dW %dE',
        \   all_non_errors,
        \   all_errors
        \)
endfunction

" some statusline stuff from fatih (https://github.com/fatih/dotfiles/blob/master/vimrc)
let s:modes = {
      \ 'n': 'NORMAL',
      \ 'i': 'INSERT',
      \ 'R': 'REPLACE',
      \ 'v': 'VISUAL',
      \ 'V': 'V-LINE',
      \ "\<C-v>": 'V-BLOCK',
      \ 'c': 'COMMAND',
      \ 's': 'SELECT',
      \ 'S': 'S-LINE',
      \ "\<C-s>": 'S-BLOCK',
      \ 't': 'TERMINAL'
      \}

let s:prev_mode = ""
function! StatusLineMode()
  let cur_mode = get(s:modes, mode(), '')
  let s:prev_mode = cur_mode
  return printf("-%s-", cur_mode)
endfunction

function! StatusLineFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! StatusLineFormat()
  return winwidth(0) > 70 ? printf("%s | %s", &ff, &fenc) : ''
endfunction

function! StatusLineFileName()
  let bnum = expand(bufnr('%'))
  let fname = '' != expand('%:t') ? expand('%:t') : '[No Name]'
  return printf("%d-%s", bnum, fname)
endfunction

" format the statusline
set statusline=
set statusline+=%{StatusLineMode()}
set statusline+=%{StatusLineFileName()}
set statusline+=%m

"" get current git status
set statusline+=\ %{fugitive#statusline()}

"" Ale status
set statusline+=%{LinterStatus()}

" right section
set statusline+=%=
" file format
set statusline+=%{StatusLineFormat()}
" file type
set statusline+=\ %{StatusLineFiletype()}
" line number
set statusline+=\ %l,
" column number
set statusline+=%2c
" % of file
set statusline+=\ %p%%
" number of lines
"set statusline+=\ %L
" ASCII and byte code under cursor
"set statusline+=\ [%03b][0x%04B]\

