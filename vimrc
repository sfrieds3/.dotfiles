" Housekeeping {{{

" reload vimrc
" :so $MYVIMRC

set nocompatible
set foldmethod=marker
set clipboard=unnamed "use system default clipboard

" turn persistant undo on
" can undo even after closing buffer/VIM
try
    set undodir=~/.vim/temp_dir/undodir
    set undofile
catch
endtry

" audo reload vimrc
"augroup myvimrc
    "au!
    "au BufWritePost .vimrc,_vimrc,vimrc,.gvimrc,_gvimrc,gvimrc so $MYVIMRC | if has('gui_running') | so $MYGVIMRC | endif
    "augroup END

"}}}

" start of plugins {{{

call plug#begin('~/.vim/plugged') " call plugged to manage plugins"

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'endwise.vim' " auto end..endif

" tpope stuff {{{

Plug 'tpope/vim-commentary' " plugin for commenting things
Plug 'tpope/vim-fugitive' " git manager for vim
Plug 'tpope/vim-surround' " advanced functions with words etc
Plug 'tpope/vim-eunuch' " unix shell commands
Plug 'tpope/vim-repeat' " adds repeat awareness- can repeat commands

" }}}

" Plug 'vim-scripts/better-whitespace' " whitespace functions

" Color time!
"Plug 'croaky/vim-colors-github'
Plug 'altercation/vim-colors-solarized'
Plug 'morhetz/gruvbox'
Plug 'tomasr/molokai'
Plug 'tpope/vim-vividchalk'
" End colors :(

Plug 'christoomey/vim-tmux-navigator' " navigate tmux and vim panes
Plug 'rking/ag.vim' " silver searcher- quick search thru code

" Language specific
Plug 'jtratner/vim-flavored-markdown' "markdown for vim
Plug 'scrooloose/syntastic' " catch-all highlighting - potential slowdown?

Plug 'tpope/vim-abolish' " coersion- (crs) snake, mixed, upper case etc
Plug 'osyo-manga/vim-over' " visual find replace

" nerd stuff
Plug 'scrooloose/nerdcommenter' " ,+c[space] to comment/uncomment lines
Plug 'scrooloose/nerdtree' " ,n to toggle nerdtree

Plug 'jiangmiao/auto-pairs' " auto pairs for brackets/parens/quotes

Plug 'ervandew/supertab' " Vim insert mode completions

" ALL PLUGINS BEFORE THIS LINE
call plug#end()

"}}}

" Basic vim setups {{{

" retain buffers until quit
set hidden

" No bells!
set visualbell

" Fast scrolling
set ttyfast

" set cursorline
set cursorline

" not case sensitive
set ignorecase
" unless all caps
set smartcase

" set characters for end of line & tab
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮

" make backspace work
set backspace=2

" path/file expansion in colon-mode
set wildmode=longest:full,list:full,list:longest
set wildchar=<TAB>

" show me where I am?
set ruler

" show filename in window titlebar
set title

set so=8 " set lines above/below cursor

set term=screen-256color " make tmux work with vim

"set autoread "set to autoread when file changed from outside

" turn on wild menu
set wildmenu

"2nd tab: complete first alternative, allow tab/S-tab to cycle back and forth
set wildmode=longest:full,full
set foldcolumn=0
" highlight extra whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

" Status line stuff
set laststatus=2
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c

"Brace face
set showmatch
set matchtime=3

" split down and right
set splitbelow
set splitright

" read filetype stuff
filetype plugin on
filetype indent on

" Time out on key codes but not mappings - needed for terminal vim?
set notimeout
set ttimeout
set ttimeoutlen=10

" resize splits when window is resized
au VimResized * :wincmd =

"set utf8 as standard encoding / en_US standard language
set encoding=utf8

" use spaces instead of tabs
set expandtab

"be smart when using tabs!
set autoindent
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4
set softtabstop=4
set wrap

" search shows all results
set incsearch
set showmatch
set hlsearch

" Line numbers 'yo'
set number
set relativenumber

" Tree style listing on Explore
let g:netrw_liststyle=3

set history=1000
set undofile
set undodir=~/.vim/undo " where to save undo history
set undolevels=1000 " How many undos
set undoreload=10000 " number of lines to save for undo
set backupdir=~/.vim/backup
set directory=~/.vim/backup

set guifont=Source\ Code\ Pro\ 12

" Color settings!
syntax enable
set background=dark
colorscheme gruvbox
let g:gruvbox_contrast_dark='hard'
let g:airline_theme='gruvbox'
"let g:airline_theme='papercolor'

" Return to last edit position when opening files
"au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal| g'\"" | endif

"}}}

" Remapping key commands {{{

"wrapped lines go down/up to next row
noremap j gj
noremap k gk

" update leader
let mapleader = ","
let g:mapleader = ","

" upper case last word using ctrl+u
inoremap <C-u> <esc>mzgUiw`za

" C-c to center cursor
nnoremap <c-c> zz

" remap 0 to first nonblank character
map 0 ^

"vertical split
nnoremap <leader>e <C-w>v<C-w>l
"horizontal split
nnoremap <leader>z <C-w>s<C-w>l

" switch windows w/ \+w
nnoremap <Leader>w <C-w><C-w>

" buffer commands
nmap <c-p> :bprevious<CR>
nmap <c-n> :bnext<CR>
"nmap bb :bw<CR>

" Move a line of text using ALT+[jk] or Command+[jk] on mac
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z
"
" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>

au TabLeave * let g:lasttab = tabpagenr()​

" Disable scrollbars (real hackers don't use scrollbars for navigation!)
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L"

" Hard to type things

iabbrev >> →
iabbrev << ←
iabbrev ^^ ↑
iabbrev VV ↓
iabbrev aa λ

" turn off nohlsarch
nmap <silent> <leader><space> :nohlsearch<CR>

"leader+S for search/replace
nnoremap <Leader>S :%s//<left>

" leader+s for search
nnoremap <Leader>s /

" switch between files with ,,
nnoremap <leader><leader> <c;^>

" Clean trailing whitespace
nnoremap <leader>W mz:%s/\s\+$//<cr>:let @/=''<cr>`z

" ,v selects text just pasted in
nnoremap <leader>v V']

" remap % to tab (to find matching bracket pairs)
nnoremap <tab> %
vnoremap <tab> %

" no arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

" Make moving through words easier!!
nnoremap W gw
nnoremap E ge

" tab for % (i.e. moving b/w brackets)
nnoremap <Tab> %

" leader enter does nothing in insert.. NOTHING!
inoremap <Leader><cr> <nop>

" sudo for write... in case you forgot :(
cmap w!! w !sudo tee % >/dev/null

" quick editing of files!
nnoremap <leader>ev :vsplit ~/.vimrc<cr>

" open/close folds the easy way
nnoremap <Leader>tf zA
"nnoremap <space> za
nnoremap <Leader>caf zM
nnoremap <Leader>af zR

" reload when switching buffer
"u FocusGained,BufEnter * :silent! ! " reload buffer when back in focus
"au FocusLost,WinLeave * :silent! noautocmd w " save file when leaving buffer

"}}}

" Language-specific configs {{{
inoremap <Leader><cr> <esc>Yp<C-a>e1C " Increment lists in markdown

"}}}

"{{{ Moving around, tabs, windows and buffers
" Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /
map <c-space> ?

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Close the current buffer
map <leader>bd :bdelete<cr>:tabclose<cr>gT

" Close all the buffers
map <leader>ba :bufdo bd<cr>

map <leader>l :bnext<cr>
map <leader>h :bprevious<cr>

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove
map <leader>t<leader> :tabnext

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab,newtab
  set stal=2
catch
endtry
"}}}

"Visual mode related {{{
"
"map // to copy visually selected text and search
vnoremap // y/<C-R>"<CR>"
"
"}}}

" highlight Interesting Words {{{

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

" plugin configurations {{{

" ignore for wild:
set wildignore+=*/tmp/*,*.so,*.swp,*.zip "macOS/Linux
set wildignore+=*/node_modules/*,*/bower_components/* "node js

" Nerdtree starts up automatially if no file selected for vim
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" ,n for nerdtree
map <Leader>o :NERDTreeToggle<CR>
" ,r to refresh NERDTree
"nmap <Leader>r :NERDTreeFocus<cr>R<c-w><c-p>
" ,O opens directory in netrw
nnoremap <leader>O :Explore %:h<cr>
"}}}
