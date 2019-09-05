" basic vim settings {{{

" initial settings {{{

let mapleader = ","
let maplocalleader = "\\"

" enable syntax
if !exists("g:syntax_on")
  syntax enable
endif

set termguicolors
set background=dark
colorscheme base16-default-dark
let base16colorspace=256  " Access colors present in 256 colorspace

noremap j gj
noremap k gk

filetype plugin on
filetype indent on

set wildmenu
set wildignorecase
set wildmode=longest:full,full
set omnifunc=syntaxcomplete#Complete
set ttyfast

" better completion
set complete=.,w,b,u,t
set completeopt=longest,menuone

" allow recursive searching for find
set path+=**

if has('path_extra')
  setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif


" do not close hidden buffers
set hidden

" set lines above/below cursor
set scrolloff=0

" timeout on key codes but not mappings
" for terminal vim
set notimeout
set ttimeout
set ttimeoutlen=10

" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

if filereadable('$HOME/.vim/autoload/pathogen.vim')
  " Use pathogen as plugin manager
  call pathogen#infect()
  call pathogen#helptags()
endif

" }}}

" basic settings {{{
set backspace=2
set matchtime=3
set encoding=utf8
set tabstop=8
set shiftwidth=4
set softtabstop=4
set clipboard=unnamed
set foldmethod=marker
set foldcolumn=0
set formatoptions=qrn1j

set autoread
set nomodeline
set visualbell
set ignorecase
set smartcase
set showmatch
set splitbelow
set splitright
set autoindent
set smartindent
set expandtab
set smarttab
set wrap
set incsearch
set showmatch
set hlsearch
set nonumber
set nocompatible

" }}}

" disable some stuff {{{

" Disable scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

" no arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

" }}}

" backup settings {{{

set undofile
set backup
set noswapfile
set undodir=~/.vim/tmp/undo// " undo files
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap// " swap files

" Make those folders automatically if they don't already exist.
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif

" }}}

" stop mouse {{{
" it's a terrible hack, but needs to be done
set mouse=a

nmap <ScrollWheelUp> <nop>
nmap <S-ScrollWheelUp> <nop>
nmap <C-ScrollWheelUp> <nop>
nmap <ScrollWheelDown> <nop>
nmap <S-ScrollWheelDown> <nop>
nmap <C-ScrollWheelDown> <nop>
nmap <ScrollWheelLeft> <nop>
nmap <S-ScrollWheelLeft> <nop>
nmap <C-ScrollWheelLeft> <nop>
nmap <ScrollWheelRight> <nop>
nmap <S-ScrollWheelRight> <nop>
nmap <C-ScrollWheelRight> <nop>

imap <ScrollWheelUp> <nop>
imap <S-ScrollWheelUp> <nop>
imap <C-ScrollWheelUp> <nop>
imap <ScrollWheelDown> <nop>
imap <S-ScrollWheelDown> <nop>
imap <C-ScrollWheelDown> <nop>
imap <ScrollWheelLeft> <nop>
imap <S-ScrollWheelLeft> <nop>
imap <C-ScrollWheelLeft> <nop>
imap <ScrollWheelRight> <nop>
imap <S-ScrollWheelRight> <nop>
imap <C-ScrollWheelRight> <nop>

vmap <ScrollWheelUp> <nop>
vmap <S-ScrollWheelUp> <nop>
vmap <C-ScrollWheelUp> <nop>
vmap <ScrollWheelDown> <nop>
vmap <S-ScrollWheelDown> <nop>
vmap <C-ScrollWheelDown> <nop>
vmap <ScrollWheelLeft> <nop>
vmap <S-ScrollWheelLeft> <nop>
vmap <C-ScrollWheelLeft> <nop>
vmap <ScrollWheelRight> <nop>
vmap <S-ScrollWheelRight> <nop>
vmap <C-ScrollWheelRight> <nop>

" }}}

"}}}

" neovim settings {{{

if has('nvim')
  " neovim commands
  " show effects of command incrementally
  set inccommand=split

  " neovim terminal - esc to exit terminal-mode
  :tnoremap <Esc> <C-\><C-n>

  " do not show line numbers in terminal mode
  augroup termsettings
    autocmd!
    autocmd TermOpen * setlocal nonumber norelativenumber
  augroup END
endif

" }}}

" statusline {{{

" set statusline color for various modes
function! InsertStatuslineColor(mode)
  if a:mode == 'i'
  hi statusline guibg=Orange ctermfg=6 guifg=Black ctermbg=0
  elseif a:mode == 'r'
  hi statusline guibg=Purple ctermfg=5 guifg=Black ctermbg=0
  else
  hi statusline guibg=DarkRed ctermfg=1 guifg=Black ctermbg=0
  endif
endfunction

set laststatus=2

function! StatusLineBuffNum()
  let bnum = expand(bufnr('%'))
  return printf("-%d-", bnum)
endfunction

function! StatusLineFiletype()
  return winwidth(0) > 160 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! StatusLineFormat()
  return winwidth(0) > 160 ? printf("%s | %s", &ff, &fenc) : ''
endfunction

function! StatusLineFileName()
  let fname = '' != expand('%:f') ? expand('%:f') : '[No Name]'
  return printf("%s", fname)
endfunction

" format the statusline
set statusline=
set statusline+=%{StatusLineBuffNum()}
set statusline+=\%{StatusLineFileName()}
set statusline+=%m

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
 "% of file
set statusline+=\ %p%%

" }}}

" general language settings {{{

augroup lang
  autocmd!

  autocmd FileType html setlocal shiftwidth=2 softtabstop=2
  autocmd FileType go setlocal shiftwidth=8 softtabstop=8
  autocmd FileType c setlocal shiftwidth=8 softtabstop=8
  autocmd FileType python setlocal shiftwidth=4 softtabstop=4
  autocmd FileType vim setlocal shiftwidth=2 softtabstop=2
  autocmd FileType ruby setlocal shiftwidth=2 softtabstop=2
  autocmd FileType eruby setlocal shiftwidth=2 softtabstop=2
augroup END

" commenting for filetypes {{{
augroup autocomment
  autocmd!
  autocmd FileType python nnoremap <buffer> <localleader>c I#<esc>
  autocmd FileType vim nnoremap <buffer> <localleader>c I"<esc>
  autocmd FileType ruby nnoremap <buffer> <localleader>c I#<esc>
  autocmd FileType eruby nnoremap <buffer> <localleader>c I#<esc>
  autocmd FileType go nnoremap <buffer> <localleader>c I//<esc>
  autocmd FileType c nnoremap <buffer> <localleader>c I//<esc>
augroup End

" uncomment
nnoremap <localleader>u ^x==

" }}}

" python {{{
augroup python
  autocmd!
  " autopep8 on gq
  autocmd FileType python setlocal formatprg=autopep8\ -

  " turn on python autocomplete
  autocmd FileType python set omnifunc=pythoncomplete#Complete

  " auto close pydoc window
  "autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
  "autocmd InsertLeave * if pumvisible() == 0|pclose|endif
augroup END
" }}}

" {{{ HTML
iabbrev </ </<C-X><C-O>
imap <C-Space> <C-X><C-O>
" }}}

" {{{ clojure

" }}}

" }}}

" custom mappings and stuff {{{

" various command shortcuts
cnoreabbrev f find
cnoreabbrev F find
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev B b
cnoreabbrev E e

" show list of digraphs -- special symbols
nnoremap <localleader>D :help digraphs<cr>:175<cr>

" upper case last word using ctrl+u
inoremap <C-u> <esc>mzgUiw`za

" Shift-Tab enters actual tab
inoremap <S-Tab> <C-V><Tab>

" stay where you are on * from fatih (http://www.github.com/fatih/dotfiles)
nnoremap <silent> * :let stay_star_view = winsaveview()<cr>*:call winrestview(stay_star_view)<cr>

" move line of text up, down using Alt-j/k
nnoremap <M-j> mz:m+<cr>`z
nnoremap <M-k> mz:m-2<cr>`z
vnoremap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" switch between files with \\
nnoremap <localleader><localleader> <c-^>

" remap % to tab (to find matching bracket pairs)
nnoremap <tab> %
vnoremap <tab> %

" use sane regex (source: https://bitbucket.org/sjl/dotfiles/src/default/vim/vimrc)
nnoremap / /\v
vnoremap / /\v

" toggle line numbers
nnoremap <silent> <Leader>n :set invnumber<cr>

" show avilable marks and be ready to swtich
nnoremap <leader>mm :<C-u>marks<cr>:normal! `

" show buffers and be ready to switch
nnoremap <silent> <leader>bb :<C-u>:buffers<cr>:buffer<space>

" Disable highlight when <leader><cr> is pressed
nnoremap <silent> <leader><cr> :nohlsearch<cr>

" higlight whitespace, but do not highlight in insert mode
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
augroup hiwhitespace
  autocmd!
  autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
  autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
  autocmd InsertLeave * match ExtraWhitespace /\s\+$/
  autocmd BufWinLeave * call clearmatches()
augroup END

" Clean trailing whitespace
nnoremap <silent> <leader>W mz:%s/\s\+$//<cr>:let @/=''<cr>`z

" Switch CWD to the directory of the open buffer
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>

" Automatically cd into the directory that the file is in
"autocmd BufEnter * execute "chdir ".escape(expand("%:p:h"), ' ')
"set autochdir

" resize splits when window is resized
augroup resize
  autocmd!
  autocmd VimResized * :wincmd =
augroup END

" netrw
nnoremap <Leader>o :Sexplore!<cr>

let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_winsize = 25

" easy editing {{{
nnoremap <leader>ev :vsplit ~/.vimrc<cr>
" }}}

" operator mappings {{{
onoremap p i(
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap il( :<c-u>normal! F)vi(<cr>
" }}}

" }}}

" {{{ functions

" quick way to open quickfix window
if !exists('*OpenQuickfix')
  function! OpenQuickfix()
    :copen
  endfunction
  command C call OpenQuickfix()
endif

" use ctrl-s to vimgrep and open results in quickfix window
if !exists('*FindAll')
  function! FindAll()
    call inputsave()
    let p = input('Enter pattern:')
    call inputrestore()
    execute 'vimgrep! "'.p.'" % | copen'
  endfunction
endif
nnoremap <C-s> :call FindAll()<cr>

" call gitgrep with :G
if !exists('*GitGrep')
  function! GitGrep(...)
    " store grepprg to restore after running
    let save = &grepprg
    " set grepprg to git grep for use in function
    set grepprg=git\ grep\ -n\ $*
    let s = 'grep!'
    let s = 'silent ' . s
    for i in a:000
      let s = s . ' ' . i
    endfor
    let s = s . ' | copen'
    execute s
    " restore grepprg to original setting
    let &grepprg = save
  endfunction
  command -nargs=+ G call GitGrep(<f-args>)
endif

" git grep for word under cursor
if !exists('*GitGrepWord')
  function GitGrepWord()
    normal! "zyiw
    call GitGrep('-w -e ', getreg('z'))
  endfunction
endif
nnoremap <C-x>G :call GitGrepWord()<cr>
" or, an easier way to do it
nnoremap <leader>G :G <cword><cr>

" grep for word under cuursor
"nnoremap <leader>g :silent execute "grep! -R " .shellescape(expand("<cWORD>")) . " ."<cr>:copen<cr>

" generate tags quickly
if !exists('*GenerateTags')
  function GenerateTags()
    :! ctags -R
  endfunction
  command T call GenerateTags()
endif

"}}}

" moving around, tabs, windows and buffers {{{

" windows {{{
" Smart way to move between windows
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l
nnoremap <leader>ww <C-w>w
nnoremap <A-o> <C-w>w
nmap <leader>w <C-W>

" resize splits
nnoremap <M-[> <C-w><
nnoremap <M-=> <C-w>+
nnoremap <M--> <C-w>-
nnoremap <M-]> <C-w>>

" }}}

" buffers and tabs {{{
" tab and buffer management
nnoremap <leader>bp :bprevious<cr>
nnoremap <leader>bn :bnext<cr>
nnoremap <leader>bd :bdelete<cr>
nnoremap <leader>tp :tabprevious<cr>
nnoremap <leader>tn :tabnext<cr>
nnoremap <leader>tt :tabnext<cr>

" Useful mappings for managing tabs
nnoremap <leader>tN :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>th :-tabmove<cr>
nnoremap <leader>tl :+tabmove<cr>

" }}}

"}}}

" highlight interesting words {{{

augroup highlight_interesting_word
  autocmd!
  " This mini-plugin provides a few mappings for highlighting words temporarily.
  "
  " Sometimes you're looking at a hairy piece of code and would like a certain
  " word or two to stand out temporarily.  You can search for it, but that only
  " gives you one color of highlighting.  Now you can use <leader>N where N is
  " a number from 1-6 to highlight the current word in a specific color.

  " credit: https://github.com/paulirish/dotfiles/blob/master/.vimrc

  function! HiInterestingWord(n) " {{{
    " Save our location.
    normal! mz

    " Yank the current word into the z register.
    normal! "zyiw

    " Calculate an arbitrary match ID.  Hopefully nothing else is using it.
    let mid = 86750 + a:n

    " Clear existing matches, but don't worry if they don't exist.
    silent! call matchdelete(mid)

    " Construct a literal pattern that has to match at boundaries.
    let pat = '\V\<' . escape(@z, '\') . '\>'

    " Actually match the words.
    call matchadd("InterestingWord" . a:n, pat, 1, mid)

    " Move back to our original location.
    normal! `z
  endfunction " }}}

  " Mappings {{{
  nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
  nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
  nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
  nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
  nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
  nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>
  " }}}

  " Default Highlights {{{
  hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=214
  hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=154
  hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=121
  hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
  hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
  hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195
  " }}}
augroup END
" }}}

" verbose debugging {{{

function! ToggleVerbose()
  if !&verbose
      set verbosefile=~/.vim/log/verbose.log
      set verbose=15
  else
      set verbose=0
      set verbosefile=
  endif
endfunction

" }}}

" notes {{{

" :helpgrep <text> - grep for <text> in all help docs
" :cn :cp to go to next or previous result from :helpgrep
" Tags:
" - Use ^] to jump to tag under cursor
" - Use g^] for ambiguous tags
" - Use ^t to jump back up the tag stack

"}}}
