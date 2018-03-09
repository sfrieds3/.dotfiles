" Housekeeping {{{

" Dependencies
" go get -u github.com/jstemmer/gotags
" go get -u github.com/nsf/gocode
" go get github.com/rogpeppe/godef
" go get -u github.com/derekparker/delve/cmd/dlv
" gometalinter --install (terminal)
" :GoInstallBinaries
" export PATH=$PATH:$(go env GOPATH)/bin
" cargo install racer
" pip install jedi
" pip3 install neovim
" pip install neovim
" :UpdateRemotePlugins
" sudo apt-get install exuberant-ctags
" go get -u github.com/sourcegraph/go-langserver

" open second tab on startup
autocmd VimEnter * TabooOpen bash
autocmd VimEnter * terminal
autocmd VimEnter * tabprevious

set nocompatible
set foldmethod=marker
set clipboard=unnamed "use system default clipboard

"set term=screen-256color " make tmux work with vim

" Update term title but restore old title after leaving Vim
set title
set titleold=

"}}}

" start of plugins {{{

call plug#begin('~/.vim/plugged') " call plugged to manage plugins"

Plug 'airblade/vim-gitgutter' "show git diff in gutter
Plug 'kien/ctrlp.vim' " C-P for searching
Plug 'gcmt/taboo.vim' " tab stuff for vim
Plug 'christoomey/vim-tmux-navigator' " navigate tmux and vim panes
Plug 'mileszs/ack.vim' " ack/ag searching in vim
Plug 'jtratner/vim-flavored-markdown' "markdown for vim
Plug 'scrooloose/syntastic' " catch-all highlighting
Plug 'osyo-manga/vim-over' " visual find replace
Plug 'scrooloose/nerdcommenter' " ,+c[space] to comment/uncomment lines
Plug 'scrooloose/nerdtree' " ,n to toggle nerdtree
Plug 'Xuyuanp/nerdtree-git-plugin' " show git status in nerdtree
Plug 'jiangmiao/auto-pairs' " auto pairs for brackets/parens/quotes
Plug 'luochen1990/rainbow' " rainbow parenthesis
Plug 'fatih/vim-go' " for golang development
Plug 'majutsushi/tagbar' " tagbar on right side
Plug 'jszakmeister/markdown2ctags' " markdown support for ctags/tagbar
Plug 'junegunn/fzf' " multiselction ui
Plug 'easymotion/vim-easymotion' " vim easymotion

"-------------------------------------------------------"

" colors
Plug 'morhetz/gruvbox'
Plug 'hickop/vim-hickop-colors'
Plug 'ayu-theme/ayu-vim'
Plug 'arcticicestudio/nord-vim'

"-------------------------------------------------------"

" language server
Plug 'autozimu/LanguageClient-neovim', {
            \ 'branch': 'next',
            \ 'do': 'bash install.sh',
            \ }

" plugins


"-------------------------------------------------------"

" deoplete
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

" deoplete sources

Plug 'zchee/deoplete-go', { 'do': 'make'} " golang autocomplete
Plug 'zchee/deoplete-jedi' " python autocomplete
Plug 'sebastianmarkow/deoplete-rust' " rust autocomplete
Plug 'Shougo/neco-vim' " vim auocomplete
Plug 'neovim/python-client' " required for python autocomplete
Plug 'davidhalter/jedi' " python autocomplete
Plug 'artur-shaik/vim-javacomplete2' " Java autocomplete
Plug 'nsf/gocode', { 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh' } " golang support

"-------------------------------------------------------"

" tpope stuff

Plug 'tpope/vim-commentary' " plugin for commenting things
Plug 'tpope/vim-fugitive' " git manager for vim
Plug 'tpope/vim-surround' " advanced functions with words etc
Plug 'tpope/vim-eunuch' " unix shell commands
Plug 'tpope/vim-repeat' " adds repeat awareness- can repeat commands
Plug 'tpope/vim-abolish' " coersion- (crs) snake, mixed, upper case etc
Plug 'tpope/vim-surround'

"-------------------------------------------------------"


" ALL PLUGINS BEFORE THIS LINE
call plug#end()

"}}}

" Language settings {{{

autocmd FileType html setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType go setlocal shiftwidth=8 tabstop=8 softtabstop=8
autocmd FileType c setlocal shiftwidth=8 tabstop=8 softtabstop=8
autocmd FileType python setlocal shiftwidth=4 tabstop=4 softtabstop=4

"-------------------------------------------------------"

" golang
" goimport on save
let g:go_fmt_command = "goimports"
let g:go_metalinter_autosave = 1
" no listchars for go files
autocmd FileType go set nolist
" show definition when hovering
let g:go_auto_type_info = 1
autocmd FileType go nnoremap <localleader>d :GoDoc<space>
autocmd FileType go nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
autocmd FileType go nnoremap <localleader>r :call LanguageClient_textDocument_rename()<CR>
autocmd FileType go nnoremap <localleader>l :GoMetaLinter<CR>

"-------------------------------------------------------"

" Java
autocmd FileType java setlocal omnifunc=javacomplete#Complete

"-------------------------------------------------------"

" Markdown
autocmd FileType markdown let b:deoplete_disable_auto_complete = 1
autocmd FileType markdown vnoremap <localleader>q gq

" }}}

" theme settings {{{

" Color settings!
if !exists("g:syntax_on")
    syntax enable
endif

"set colorscheme below
colorscheme nord
highlight LineNr ctermfg=NONE ctermbg=NONE guifg=NONE guibg=NONE " no highlighting for line number
highlight MatchParen ctermfg=black ctermbg=white guifg=black guifg=white
highlight Todo ctermfg=255 ctermbg=NONE guifg=#ffff00 guibg=NONE

" nord colorscheme settings
if g:colors_name == 'nord'
    let g:nord_italic = 1
    let g:nord_italic_comments = 1
    highlight Comment guifg=#D08770 " comment colors
    " show line at column 80, full highlight from column 120 on
    "let &colorcolumn="80,".join(range(120,999),",")
endif

if g:colors_name == 'hickop'
    " show line at column 80, full highlight from column 120 on
    highlight ColorColumn ctermbg=235 guibg=#282828
    let &colorcolumn="80,".join(range(120,999),",")
endif

" }}}

" statusline {{{

set laststatus=2
function! InsertStatuslineColor(mode)
  if a:mode == 'i'
    hi statusline guibg=Cyan ctermfg=6 guifg=Black ctermbg=0
  elseif a:mode == 'r'
    hi statusline guibg=Purple ctermfg=5 guifg=Black ctermbg=0
  else
    hi statusline guibg=DarkRed ctermfg=1 guifg=Black ctermbg=0
  endif
endfunction

au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi statusline guibg=DarkGrey ctermfg=8 guifg=black ctermbg=15

" default the statusline to dark grey when entering Vim
hi statusline guibg=DarkGrey ctermfg=8 guifg=black ctermbg=15

" Formats the statusline
set statusline=%F                           " file name
set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
set statusline+=%{&ff}] "file format
set statusline+=%y      "filetype

" get current git status
set statusline+=%{fugitive#statusline()}

" show syntastic warnings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

set statusline+=\ %=                        " align left
set statusline+=%l/%L[%p%%]            " line X of Y [percent of file]
set statusline+=\ C:%2c                    " current column
set statusline+=\ B:%n                    " Buffer number
set statusline+=\ [%03b][0x%04B]\               " ASCII and byte code under cursor

" end statusline

" }}}

" Basic vim settings {{{
"
let mapleader = ","                 " update leader
let maplocalleader = "\\"           " map local leader- can use for other commands with <localleader>

" blinking cursor
set guicursor=a:blinkon1

"wrapped lines go down/up to next row
noremap j gj
noremap k gk

" retain buffers until quit
set hidden

" No bells!
set visualbell

" Fast scrolling
set ttyfast

" set cursorline for current window only
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END

" allow recursive searching with :find
set path+=**

" Create the `tags` file (may need to install ctags first)
command! MakeTags !ctags -R .

" NOW WE CAN:
" - Use ^] to jump to tag under cursor
" - Use g^] for ambiguous tags
" - Use ^t to jump back up the tag stack

" not case sensitive
set ignorecase
" unless all caps
set smartcase

" set characters for end of line & tab
"set list
set showbreak=↪
set listchars=tab:\|_,nbsp:␣,extends:…,precedes:…

" make backspace work
set backspace=2

" path/file expansion in colon-mode
set wildmode=longest:full,list:full,list:longest
set wildchar=<TAB>

" show me where I am?
set ruler

" show filename in window titlebar
set title

set so=7 " set lines above/below cursor

"set autoread "set to autoread when file changed from outside

" turn on wild menu
set wildmenu

"2nd tab: complete first alternative, allow tab/S-tab to cycle back and forth
set wildmode=longest:full,full
set foldcolumn=0

" do not highlight in insert mode
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" auto line wrap at 80 for .md and .html files
" select line and gq to reformat
au BufRead,BufNewFile *.md setlocal textwidth=80
au BufRead,BufNewFile *.html setlocal textwidth=80

"Brace face
set showmatch
set matchtime=3

" split down and right
set splitbelow
set splitright

" filetype
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

" Line numbers
set nonumber

" netrw settings
" - :edit a folder to open a file browser
" - <CR>/v/t to open in an h-split/v-split/tab
" - check |netrw-browse-maps| for more mappings
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=4  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

if !isdirectory($HOME."/.vim/backup")
    silent! execute "!mkdir ~/.vim/backup"
endif

if !isdirectory($HOME."/.vim/undo")
    silent! execute "~mkdir !/.vim/undo"
endif

set history=1000
set undofile
set undodir=~/.vim/undo " where to save undo history
set undolevels=1000 " How many undos
set undoreload=10000 " number of lines to save for undo
set backupdir=~/.vim/backup
set noswapfile

"}}}

" neovim settings {{{

if has('nvim')
" neovim commands
" neovim terminal - esc to exit terminal-mode
:tnoremap <Esc> <C-\><C-n>

" set termguicolors
set termguicolors

" do not show line numbers in terminal mode
au TermOpen * setlocal nonumber norelativenumber

" use Alt+{hjkl} to navigate windows from terminal mode
:tnoremap <A-h> <C-\><C-N><C-w>h
:tnoremap <A-j> <C-\><C-N><C-w>j
:tnoremap <A-k> <C-\><C-N><C-w>k
:tnoremap <A-l> <C-\><C-N><C-w>l
endif

" }}}

" Remapping key commands {{{

" upper case last word using ctrl+u
inoremap <C-u> <esc>mzgUiw`za

" Shift-Tab enters actual tab
inoremap <S-Tab> <C-V><Tab>

" remap 0 to first nonblank character
nnoremap 0 ^

" vertical split
nnoremap <leader>\| <C-w>v

" horizontal split
nnoremap <leader>- <C-w>s

" switch windows w/ \+w
nnoremap <Leader>w <C-w><C-w>

" move line of text up
nnoremap <C-M-j> mz:m+<cr>`z

" move line ot text down
nnoremap <C-M-k> mz:m-2<cr>`z

" move line of text up (visual mode)
vnoremap <C-M-j> :m'>+<cr>`<my`>mzgv`yo`z

" move line of text down (visual mode)
vnoremap <C-M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" turn off nohlsearch
nmap <silent> <leader><space> :nohlsearch<CR>

" leader+S for search/replace
nnoremap <Leader>S :%s//<left>

" leader+s for search
nnoremap <Leader>s /

" switch between files with ,,
nnoremap <leader><leader> <c;^>

" Clean trailing whitespace
nnoremap <silent> <leader>W mz:%s/\s\+$//<cr>:let @/=''<cr>`z

" ,v selects text just pasted in
nnoremap <leader>v V']

" remap % to tab (to find matching bracket pairs)
nnoremap <tab> %
vnoremap <tab> %

" leader enter does nothing in insert
inoremap <Leader><cr> <nop>

" Close all but the current one
nnoremap <localleader>o :only<CR>

" sudo for write
cmap w!! w !sudo tee % >/dev/null

" toggle line numbers
nnoremap <silent> <Leader>n :set invnumber<CR>

" C-n to page down
nnoremap <C-n> <C-f>M

" C-b to page up
nnoremap <C-b> <C-b>M

" open Ack quick fix window to show TODO's
nnoremap <silent> <leader>vt :Ack! TODO<CR>

" open Ack quick fix winow to show currnet word
nnoremap <leader>A :Ack! <cword><CR>

" ack for a word
nnoremap <leader>a :Ack!

" <space> to show avilable marks and be ready to swtich
nnoremap <silent> <space> :<C-u>marks<CR>:normal! `

" map // to copy visually selected text and search
vnoremap // y/<C-R>"<CR>

" Switch CWD to the directory of the open buffer
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>

" Disable highlight when <leader><cr> is pressed
nnoremap <silent> <leader><cr> :noh<cr>

" <localleader>b to list buffers
nnoremap <silent> <localleader>b :ls b<cr>

" use ctrl-s to vimgrep and open results in quickfix window
function! FindAll()
    call inputsave()
    let p = input('Enter pattern:')
    call inputrestore()
    execute 'vimgrep "'.p.'" % |copen'
endfunction
nnoremap <C-s> :call FindAll()<cr>

" <localleader>t to show tagbar
nnoremap <localleader>t :TagbarToggle<CR>

" quick editing of files
nnoremap <leader>ev :vsplit ~/.vimrc<cr>

" fold controls
nnoremap <Leader>tf zA
"nnoremap <space> za
nnoremap <Leader>caf zM
nnoremap <Leader>af zR

" no arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

" Disable scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L"

"}}}

"{{{ Moving around, tabs, windows and buffers

" use Alt+{hjkl} to navigate windows from any mode
:inoremap <A-h> <C-\><C-N><C-w>h
:inoremap <A-j> <C-\><C-N><C-w>j
:inoremap <A-k> <C-\><C-N><C-w>k
:inoremap <A-l> <C-\><C-N><C-w>l
:nnoremap <A-h> <C-w>h
:nnoremap <A-j> <C-w>j
:nnoremap <A-k> <C-w>k
:nnoremap <A-l> <C-w>l

" Smart way to move between windows
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l

" Close the current buffer
nnoremap <leader>bd :bdelete<cr>

" go to previous and next buffer
nnoremap <C-Left> :bprevious<cr>
nnoremap <C-Right> :bnext<cr>

" Useful mappings for managing tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>th :-tabmove<cr>
nnoremap <leader>tl :+tabmove<cr>
nnoremap <leader>[ :tabprevious<cr>
nnoremap <leader>] :tabnext<cr>
nnoremap <leader>< :tabprevious<cr>
nnoremap <leader>> :tabnext<cr>

" rename tab
nnoremap <leader>tr :TabooRename<space>
nnoremap <leader>T :TabooOpen<space>

" resize splits with ctrl shift hjkl
nnoremap <M-h> <C-w><
nnoremap <M-j> <C-w>+
nnoremap <M-k> <C-w>-
nnoremap <M-l> <C-w>>

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" Opens a new tab with the current buffer's path
nnoremap <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" Specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab,newtab
  set stal=2
catch
endtry

"}}}

" Plugin Configurations {{{

" deoplete
let g:deoplete#enable_at_startup = 1
autocmd CompleteDone * pclose!
"
" deoplete-go settings
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

" make enter work with deoplete in insert mode
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function() abort
    return deoplete#close_popup() . "\<CR>"
endfunction

" LanguageClient
let g:LanguageClient_serverCommands = {
            \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
            \ 'python': ['pyls'],
            \ 'cpp': ['clangd'],
            \ 'go': ['go-langserver'],
            \ }

let g:LanguageClient_autoStart = 1


" ignore for wild:
set wildignore+=*/tmp/*,*.so,*.swp,*.zip "macOS/Linux
set wildignore+=*/node_modules/*,*/bower_components/* "node js

" Git gutter
nnoremap <Leader>gg :GitGutterLineHighlightsToggle<CR>
nnoremap <Leader>gn :GitGutterNextHunk<CR>
nnoremap <Leader>gp :GitGutterPrevHunk<CR>
let g:gitgutter_override_sign_column_highlight = 0 " don't highlight

" syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:go_list_type = "quickfix"

" use [c and ]c to cycle throguh hunks in all buffers
function! NextHunkAllBuffers()
  let line = line('.')
  GitGutterNextHunk
  if line('.') != line
    return
  endif

  let bufnr = bufnr('')
  while 1
    bnext
    if bufnr('') == bufnr
      return
    endif
    if !empty(GitGutterGetHunks())
      normal! 1G
      GitGutterNextHunk
      return
    endif
  endwhile
endfunction

function! PrevHunkAllBuffers()
  let line = line('.')
  GitGutterPrevHunk
  if line('.') != line
    return
  endif

  let bufnr = bufnr('')
  while 1
    bprevious
    if bufnr('') == bufnr
      return
    endif
    if !empty(GitGutterGetHunks())
      normal! G
      GitGutterPrevHunk
      return
    endif
  endwhile
endfunction

nmap <silent> ]c :call NextHunkAllBuffers()<CR>
nmap <silent> [c :call PrevHunkAllBuffers()<CR>

" fugitive
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>GP :Gpush<CR>
nnoremap <Leader>gb :Gblame

" NERDTree

" Nerdtree starts up automatially if no file selected for vim
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" ,o for nerdtree
map <Leader>o :NERDTreeToggle<CR>

" let nerdtree show hidden files by default (I to toggle)
let NERDTreeShowHidden=1

" ,or to refresh NERDTree
"nmap <Leader>or :NERDTreeFocus<cr>R<c-w><c-p>

" open nerdtree in current directory with <leader>o
 map <leader>i :NERDTreeFind<cr>

" ,O opens directory in netrw
nnoremap <Leader>O :Explore %:h<cr>

" use ag for ack search, fall back on ack if ag not avail
if executable('ag')
      let g:ackprg = 'ag --vimgrep'
  endif

" ack/ag with <leader> a
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ack!<Space>

" use ag as default ack client
let g:ackprg = 'ag --nogroup --nocolor --column'

" ctrlp mappings
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
nnoremap <C-e> :CtrlPBuffer<CR>
nnoremap <Leader>bf :CtrlPBuffer<CR>
nnoremap <Leader>p :CtrlPMixed<CR>

" tagbar
let g:tagbar_type_go = {
            \ 'ctagstype' : 'go',
            \ 'kinds'     : [
            \ 'p:package',
            \ 'i:imports:1',
            \ 'c:constants',
            \ 'v:variables',
            \ 't:types',
            \ 'n:interfaces',
            \ 'w:fields',
            \ 'e:embedded',
            \ 'm:methods',
            \ 'r:constructor',
            \ 'f:functions'
            \ ],
            \ 'sro' : '.',
            \ 'kind2scope' : {
            \ 't' : 'ctype',
            \ 'n' : 'ntype'
            \ },
            \ 'scope2kind' : {
            \ 'ctype' : 't',
            \ 'ntype' : 'n'
            \ },
            \ 'ctagsbin'  : 'gotags',
            \ 'ctagsargs' : '-sort -silent'
            \ }

" rust
let g:tagbar_type_rust = {
            \ 'ctagstype' : 'rust',
            \ 'kinds' : [
            \'T:types,type definitions',
            \'f:functions,function definitions',
            \'g:enum,enumeration names',
            \'s:structure names',
            \'m:modules,module names',
            \'c:consts,static constants',
            \'t:traits',
            \'i:impls,trait implementations',
            \]
            \}

" markdown
" Add support for markdown files in tagbar.
let g:tagbar_type_markdown = {
            \ 'ctagstype': 'markdown',
            \ 'ctagsbin' : '~/.vim/plugged/markdown2ctags/markdown2ctags.py',
            \ 'ctagsargs' : '-f - --sort=yes',
            \ 'kinds' : [
            \ 's:sections',
            \ 'i:images'
            \ ],
            \ 'sro' : '|',
            \ 'kind2scope' : {
            \ 's' : 'section',
            \ },
            \ 'sort': 0,
            \ }

" rainbow parenthesis
" set colors for rainbow parenthesis
let faded_orange = '#af3a03'
let faded_blue = '#076678'
let faded_yellow = '#b57614'
let faded_green = '#79740e'
let faded_aqua = '#427b58'
let clemson_orange = '#F66733'
let clemson_regalia = '#522D80'
let clemson_hartwell_moon = '#D4C99E'
let clemson_howards_rock = '#685C53'
let clemson_calhoun_fields = '#B5C327'

" rainbow parenthesis settings
nnoremap <Leader>r :RainbowToggle<CR> " turn rainbow parenthesis on
let g:rainbow_active = 0
let g:rainbow_conf = {
            \	'guifgs': [clemson_orange, clemson_regalia, clemson_hartwell_moon, clemson_howards_rock, clemson_calhoun_fields],
            \	'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
            \	'operators': '_,_',
            \	'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
            \	'separately': {
            \		'*': {},
            \		'tex': {
            \			'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
            \		},
            \		'lisp': {
            \			'guifgs': ['white', 'royalblue3', 'darkorange3', 'seagreen3', 'darkorchid3'],
            \		},
            \		'vim': {
            \			'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
            \		},
            \		'html': {
            \			'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
            \		},
            \		'css': 0,
            \	}
            \}

"}}}

" Highlight Interesting Words {{{

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

" COVTIL (Cool Other Vim Things I Learned) {{{
" SOURCE: https://www.youtube.com/watch?v=XA2WjJbmmoM
" https://github.com/mcantor/no_plugins/blob/master/no_plugins.vim

" can use :b to autocomplete to any open buffer
" :ls will list all open buffers

" AUTOCOMPLETE:

" The good stuff is documented in |ins-completion|

" HIGHLIGHTS:
" - ^x^n for JUST this file
" - ^x^f for filenames (works with our path trick!)
" - ^x^] for tags only
" - ^n for anything specified by the 'complete' option

" NOW WE CAN:
" - Use ^n and ^p to go back and forth in the suggestion list

" :helpgrep <text> - grep for <text> in all help docs
" :cn :cp to go to next or previous result from :helpgrep
"}}}
