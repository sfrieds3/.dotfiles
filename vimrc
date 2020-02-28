" vim settings {{{

" plugins {{{
" bootstrap vim plugged
if has("unix")
  if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
          \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif
endif

call plug#begin('~/.vim/plugged')

"plugins go here
Plug 'kovisoft/slimv'
Plug 'sjl/badwolf'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'itchyny/lightline.vim'
Plug 'majutsushi/tagbar'
Plug 'ctrlpvim/ctrlp.vim'

call plug#end()

" }}}

" initial settings {{{

let mapleader = ","
let maplocalleader = "\\"

" enable syntax
if !exists("g:syntax_on")
  syntax enable
endif

set termguicolors
set background=dark
colorscheme badwolf
"let base16colorspace=256  " Access colors present in 256 colorspace

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
set complete=.,w,b,u,t,i
set completeopt=longest,menuone

set path=,,

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

" }}}

" basic settings {{{
set laststatus=2
set backspace=2
set matchtime=3
set encoding=utf8
set tabstop=4
set shiftwidth=4
set softtabstop=4
set clipboard=unnamed
set foldmethod=marker
set foldcolumn=0
set formatoptions=qrn1j
set colorcolumn=80

set cursorline
set showcmd
set autoread
set nomodeline
set visualbell
set ignorecase
set smartcase
set showmatch
set splitbelow
set splitright
set autoindent
set expandtab
set smarttab
set wrap
set incsearch
set showmatch
set hlsearch
set nonumber
set nocompatible

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

" plugin configs {{{

 " lightline {{{

 let g:lightline = {
             \ 'active': {
             \   'left': [ [ 'mode', 'paste' ],
             \             [ 'pwd', 'gitbranch', 'readonly', 'filename', 'modified' ] ]
             \ },
             \ 'component_function': {
             \   'gitbranch': 'fugitive#head'
             \ },
             \ }
 
 " }}}

" ale {{{

"let g:ale_lint_on_text_changed = 'never'
"let g:ale_lint_on_enter = 0
"
"nnoremap <space>n :lnext<CR>
"nnoremap <space>p :lprevious<CR>
"nnoremap <space>r :lrewind<CR>
"
"" and use a simpler warning
"let g:ale_sign_warning = '∘'
"" set erorr sign
"let g:ale_sign_error = '●'
"
"" update error msg
"let g:ale_echo_msg_error_str = 'E'
"let g:ale_echo_msg_warning_str = 'W'
"let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
"
"" ignore annoying erorrs
"let g:ale_python_flake8_options = '--ignore=E501'

" }}}

" jedi {{{
let g:jedi#show_call_signatures=2

" }}}

" slimv {{{
let g:slimv_repl_split=0
let g:slimv_repl_simple_eval=0
let g:paredit_mode=0
" TODO: write function to allow us to change this on the fly
let g:slimv_impl = 'clisp'

"}}}

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
  
  "autopep8 on gq
  autocmd FileType python setlocal formatprg=autopep8\ -
augroup END

" python {{{
augroup python
  autocmd!
  " open quickfix with list of functions
  nnoremap <silent> <localleader>f :exec("vimgrep /def /j %")<cr> :exec("copen")<cr>
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
nnoremap <localleader>D :help digraphs<cr>:179<cr>zt

" toggle line and column markers
nnoremap <silent> <localleader>c :exec("set cursorcolumn!")<cr>
nnoremap <silent> <localleader>r :exec("set cursorline!")<cr>

" upper case last word using ctrl+u
inoremap <C-u> <esc>mzgUiw`za

" Shift-Tab enters actual tab
inoremap <S-Tab> <C-V><Tab>

" stay where you are on * from fatih (http://www.github.com/fatih/dotfiles)
nnoremap <silent> * :let stay_star_view = winsaveview()<cr>*:call winrestview(stay_star_view)<cr>

" move line of text up/down
nnoremap <C-Down> mz:m+<cr>`z
nnoremap <C-Up> mz:m-2<cr>`z
vnoremap <C-Down> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <C-Up> :m'<-2<cr>`>my`<mzgv`yo`z

nnoremap <localleader><localleader> :exec("Tagbar")<cr>

" use sane regex (source: https://bitbucket.org/sjl/dotfiles/src/default/vim/vimrc)
nnoremap / /\v
vnoremap / /\v

" toggle line numbers
nnoremap <silent> <Leader>n :set invnumber<cr>

" show avilable marks and be ready to swtich
nnoremap <leader>mm :<C-u>marks<cr>:normal! `

" show buffers and be ready to switch
nnoremap <silent> <localleader>bb :<C-u>:buffers<cr>:buffer<space>

" Disable highlight
nnoremap <silent> <leader><cr> :nohlsearch<cr>

" higlight whitespace, but do not highlight in insert mode
"highlight ExtraWhitespace ctermbg=red guibg=red
"match ExtraWhitespace /\s\+$/
"augroup hiwhitespace
"  autocmd!
"  autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
"  autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
"  autocmd InsertLeave * match ExtraWhitespace /\s\+$/
"  autocmd BufWinLeave * call clearmatches()
"augroup END

" Clean trailing whitespace
nnoremap <silent> <leader>W mz:%s/\s\+$//<cr>:let @/=''<cr>`z

" Switch CWD to the directory of the open buffer
nnoremap <localleader>cd :cd %:p:h<cr>:pwd<cr>

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
let g:netrw_winsize = 25

" easy editing {{{
nnoremap <leader>ev :vsplit ~/.vimrc<cr>
nnoremap <silent> <leader>es :source ~/.vimrc<cr> :echo "sourced ~/.vimrc"<cr>
nnoremap <leader><space> :split ~/todo<cr>
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
nnoremap <leader>q :call OpenQuickfix()<cr>

" use ctrl-s to vimgrep and open uesults in quickfix window
if !exists('*FindAll')
  function! FindAll()
    call inputsave()
    let p = input('Enter pattern:')
    call inputrestore()
    execute 'vimgrep! "'.p.'" % | copen'
  endfunction
endif
"nnoremap <leader>s :call FindAll()<cr>
"nnoremap <leader>S :call FindAll()<cr><cword><cr>

" gitgrep
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
  command -nargs=+ GitGrep call GitGrep(<f-args>)
endif

" git grep for word under cursor
if !exists('*GitGrepWord')
  function GitGrepWord()
    normal! "zyiw
    call GitGrep('-w -e ', getreg('z'))
  endfunction
endif
nnoremap <C-x>G :call GitGrepWord()<cr>

" grep for word under cursor
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
nmap <leader>w <C-W>

" resize splits
nnoremap <C-S-Left> <C-w><
nnoremap <C-S-Up> <C-w>+
nnoremap <C-S-Down> <C-w>-
nnoremap <C-S-Right> <C-w>>

" }}}

" buffers and tabs {{{
" tab and buffer management
nnoremap <localleader>bp :bprevious<cr>
nnoremap <localleader>bn :bnext<cr>
nnoremap <localleader>bd :bdelete<cr>
nnoremap <C-left> :bprevios<cr>
nnoremap <C-right> :bnext<cr>
nnoremap <localleader>tp :tabprevious<cr>
nnoremap <localleader>tn :tabnext<cr>
nnoremap <localleader>tt :tabnext<cr>
nnoremap <C-left> :tabprevious<cr>
nnoremap <C-right> :tabnext<cr>

" Useful mappings for managing tabs
nnoremap <localleader>tN :tabnew<cr>
nnoremap <localleader>to :tabonly<cr>
nnoremap <localleader>tc :tabclose<cr>
nnoremap <localleader>th :-tabmove<cr>
nnoremap <localleader>tl :+tabmove<cr>

" }}}

"}}}

" highlight interesting words {{{

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
