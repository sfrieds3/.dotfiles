" housekeeping {{{

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
" pip install websocket-client sexpdata (ensime)

" :helpgrep <text> - grep for <text> in all help docs
" :cn :cp to go to next or previous result from :helpgrep

"}}}

" start of plugins {{{

" install plugged if not installed
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged') " call plugged to manage plugins"

Plug 'airblade/vim-gitgutter' "show git diff in gutter
Plug 'christoomey/vim-tmux-navigator' " navigate tmux and vim panes
Plug 'easymotion/vim-easymotion' " vim easymotion
Plug 'fatih/vim-go' " for golang development
Plug 'gcmt/taboo.vim' " tab stuff for vim
Plug 'jszakmeister/markdown2ctags' " markdown support for ctags/tagbar
Plug 'majutsushi/tagbar' " tagbar on right side
Plug 'mileszs/ack.vim' " ack/ag searching in vim
Plug 'osyo-manga/vim-over' " visual find replace
Plug 'scrooloose/nerdcommenter' " ,+c[space] to comment/uncomment lines
Plug 'scrooloose/nerdtree' " ,n to toggle nerdtree
Plug 'tpope/vim-fugitive' " git manager for vim
Plug 'tpope/vim-surround' " advanced functions with words etc
Plug 'w0rp/ale' " linting
Plug 'xuyuanp/nerdtree-git-plugin' " show git status in nerdtree

"-------------------------------------------------------"

" fzf- fuzzy file finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

"-------------------------------------------------------"

" colors
Plug 'arcticicestudio/nord-vim'
Plug 'ayu-theme/ayu-vim'
Plug 'morhetz/gruvbox'
Plug 'NLKNguyen/papercolor-theme'
Plug 'sfrieds3/vim-hickop-colors'
Plug 'sjl/badwolf'

"-------------------------------------------------------"

" language server
Plug 'autozimu/LanguageClient-neovim', {
            \ 'branch': 'next',
            \ 'do': 'bash install.sh',
            \ }

""-------------------------------------------------------"

"" deoplete
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

"" deoplete sources

Plug 'Shougo/neco-vim' " vim auocomplete
Plug 'artur-shaik/vim-javacomplete2' " Java autocomplete
Plug 'davidhalter/jedi' " python autocomplete
Plug 'neovim/python-client' " required for python autocomplete
Plug 'nsf/gocode', { 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh' } " golang support
Plug 'zchee/deoplete-go', { 'do': 'make'} " golang autocomplete
Plug 'zchee/deoplete-jedi' " python autocomplete

"" Scala stuff
Plug 'ensime/ensime-vim', { 'do': ':UpdateRemotePlugins' } " Scala
Plug 'derekwyatt/vim-scala'

" ALL PLUGINS BEFORE THIS LINE
call plug#end()

"}}}

" language settings {{{

autocmd FileType html setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType go setlocal shiftwidth=8 tabstop=8 softtabstop=8
autocmd FileType c setlocal shiftwidth=8 tabstop=8 softtabstop=8
autocmd FileType python setlocal shiftwidth=4 tabstop=4 softtabstop=4
autocmd FileType scala setlocal shiftwidth=2 tabstop=2 softtabstop=2

"-------------------------------------------------------"

" golang
" no warning for out of date nvim
let g:go_version_warning = 0
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
" python


"-------------------------------------------------------"
" Java
autocmd FileType java let b:deoplete_disable_auto_complete = 1
autocmd FileType java let g:EclimCompletionMethod = 'omnifunc'

"-------------------------------------------------------"
" Scala
autocmd BufWritePost *.scala silent :EnTypeCheck
autocmd FileType scala nnoremap <localleader>k :EnType<CR>
autocmd FileType scala nnoremap <localleader>df :EnDeclaration<CR>
"Linting with neomake
let g:neomake_sbt_maker = {
      \ 'exe': 'sbt',
      \ 'args': ['-Dsbt.log.noformat=true', 'compile'],
      \ 'append_file': 0,
      \ 'auto_enabled': 1,
      \ 'output_stream': 'stdout',
      \ 'errorformat':
          \ '%E[%trror]\ %f:%l:\ %m,' .
            \ '%-Z[error]\ %p^,' .
            \ '%-C%.%#,' .
            \ '%-G%.%#'
     \ }
let g:neomake_enabled_makers = ['sbt']
let g:neomake_verbose=3
" Neomake on text change
"autocmd InsertLeave,TextChanged * update | Neomake! sbt

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
"colorscheme badwolf
colorscheme desert
highlight LineNr ctermfg=NONE ctermbg=NONE guifg=NONE guibg=NONE " no highlighting for line number
highlight MatchParen ctermfg=black ctermbg=white guifg=black guifg=#f66733
highlight Todo ctermfg=255 ctermbg=NONE guifg=#ffff00 guibg=NONE

" desert colorscheme settings
if g:colors_name == 'desert'
    highlight NonText guibg=grey20
    highlight VertSplit guibg=grey20
    let g:gitgutter_override_sign_column_highlight = 0
    highlight SignColumn guibg=grey20
    highlight GitGutterAdd guibg=grey20
    highlight GitGutterChange guibg=grey20
    highlight GitGutterDelete guibg=grey20
    highlight GitGutterChangeDelete guibg=grey20
endif

" nord colorscheme settings
if g:colors_name == 'nord'
    let g:nord_italic = 1
    let g:nord_italic_comments = 1
    highlight Comment guifg=#D08770 " comment colors
    " show line at column 80, full highlight from column 120 on
    "let &colorcolumn="80,".join(range(120,999),",")
endif

" hickop colorscheme settings
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

function! LinterStatus() abort
    let l:counts = ale#statusline#Count(bufnr(''))

    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors

    return l:counts.total == 0 ? 'OK' : printf(
    \   '%dW %dE',
    \   all_non_errors,
    \   all_errors
    \)
endfunction


au InsertEnter * call InsertStatuslineColor(v:insertmode)
au InsertLeave * hi statusline guibg=DarkGrey ctermfg=8 guifg=black ctermbg=15

" default the statusline to dark grey when entering Vim
hi statusline guibg=DarkGrey ctermfg=8 guifg=black ctermbg=15

set modeline

" format the statusline
set statusline=%f " file name
set statusline+=%m " modified flag
set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
set statusline+=%{&ff}] "file format

"" get current git status
set statusline+=%{fugitive#statusline()}

"" Ale status
set statusline+=%{LinterStatus()}

set statusline+=\ %=                        " align left
set statusline+=%l/%L[%p%%]            " line X of Y [percent of file]
set statusline+=\ C:%2c                    " current column
set statusline+=\ B:%n                    " Buffer number
set statusline+=\ [%03b][0x%04B]\               " ASCII and byte code under cursor

" end statusline

" }}}

" basic vim settings {{{

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

" not case sensitive, unless all caps
set ignorecase
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

"set utf8 as standard encoding
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

set nocompatible
set foldmethod=marker
set clipboard=unnamed "use system default clipboard

" Update term title but restore old title after leaving Vim
set title
set titleold=

" mouse no work - also put in ~/.config/nvim/init.vim
set mouse=a

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

" Specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab,newtab
  set stal=2
catch
endtry

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
":tnoremap <A-h> <C-\><C-N><C-w>h
":tnoremap <A-j> <C-\><C-N><C-w>j
":tnoremap <A-k> <C-\><C-N><C-w>k
":tnoremap <A-l> <C-\><C-N><C-w>l
endif

" }}}

" remapping key commands {{{

" show list of digraphs -- special symbols
nnoremap <localleader>D :help digraphs<cr>:175<cr>

" upper case last word using ctrl+u
inoremap <C-u> <esc>mzgUiw`za

" Shift-Tab enters actual tab
inoremap <S-Tab> <C-V><Tab>

" remap 0 to first nonblank character
nnoremap 0 ^

" switch windows w/ ,+w
nnoremap <Leader>w <C-w><C-w>

" move line of text up, down using Alt-j/k
nnoremap <M-j> mz:m+<cr>`z
nnoremap <M-k> mz:m-2<cr>`z
vnoremap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" leader+S for search/replace
nnoremap <Leader>S :OverCommandLine<cr>%s/

" switch between files with \\
nnoremap <localleader><localleader> <c-^>

" Clean trailing whitespace
nnoremap <silent> <leader>W mz:%s/\s\+$//<cr>:let @/=''<cr>`z

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

" open Ack quick fix window to show TODO's
nnoremap <silent> <leader>vt :Ack! TODO<CR>

" open Ack quick fix winow to show currnet word
nnoremap <leader>A :Ack! <cword><CR>

" ack for a word
nnoremap <leader>a :Ack!

" <space> to show avilable marks and be ready to swtich
nnoremap <silent> <space> :<C-u>marks<CR>:normal! `

" Switch CWD to the directory of the open buffer
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>

" Disable highlight when <leader><cr> is pressed
nnoremap <silent> <leader><cr> :nohlsearch<cr>

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
nnoremap <leader>ev :vsplit ~/.dotfiles/vimrc<cr>

" fold controls
nnoremap <Leader>tf zA
nnoremap <Leader>caf zM
nnoremap <Leader>af zR

"}}}

" moving around, tabs, windows and buffers {{{

" Smart way to move between windows
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l

" Close the current buffer
nnoremap <leader>bd :bdelete<cr>

" go to previous and next buffer
nnoremap <leader>bp :bprevious<cr>
nnoremap <leader>bn :bnext<cr>

" <leader>lb to list buffers
nnoremap <silent> <leader>lb :ls b<cr>

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

" resize splits
nnoremap <C-M-h> <C-w><
nnoremap <C-M-j> <C-w>+
nnoremap <C-M-k> <C-w>-
nnoremap <C-M-l> <C-w>>

"}}}

" plugin configurations {{{

" deoplete
let g:deoplete#enable_at_startup = 1
"autocmd CompleteDone * pclose!
let g:deoplete#omni_patterns = {}
let g:deoplete#omni_patterns.scala = '[^. *\t]\.\w*\|: [A-Z]\w*'

" make enter work with deoplete in insert mode
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function() abort
    return deoplete#close_popup() . "\<CR>"
endfunction

" LanguageClient
let g:LanguageClient_serverCommands = {
            \ 'python': ['pyls'],
            \ 'cpp': ['clangd'],
            \ 'c': ['clangd'],
            \ 'go': ['go-langserver'],
            \ }

let g:LanguageClient_autoStart = 1
nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>art = 1

" cquery language client rgister (if installed)
if executable('cquery')
   au User lsp_setup call lsp#register_server({
      \ 'name': 'cquery',
      \ 'cmd': {server_info->['cquery']},
      \ 'root_uri': {server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'compile_commands.json'))},
      \ 'initialization_options': { 'cacheDirectory': '/path/to/cquery/cache' },
      \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp'],
      \ })
endif

" ignore for wild:
set wildignore+=*/tmp/*,*.so,*.swp,*.zip "macOS/Linux
set wildignore+=*/node_modules/*,*/bower_components/* "node js

" Git gutter
nnoremap <Leader>gg :GitGutterToggle<CR>
nnoremap <Leader>gh :GitGutterLineHighlightsToggle<CR>
nnoremap <Leader>gn :GitGutterNextHunk<CR>
nnoremap <Leader>gp :GitGutterPrevHunk<CR>
let g:gitgutter_override_sign_column_highlight = 0 " don't highlight

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
nnoremap <Leader>gd :Gdiff<CR>

" vim-over visual find replace
nnoremap <leader>vr :OverCommandLine<CR>%s/

" NERDTree

" Nerdtree starts up automatially if no file selected for vim
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" ,o for nerdtree
map <Leader>o :NERDTreeToggle<CR>

" let nerdtree show hidden files by default (I to toggle)
let NERDTreeShowHidden=1

" ,or to refresh NERDTree
"nmap <Leader>or :NERDTreeFocus<cr>R<c-w><c-p>

" open nerdtree in current directory with <leader>i
 map <leader>i :NERDTreeFind<cr>

" custom indicator map
let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "ᵐ",
    \ "Staged"    : "ˢ",
    \ "Untracked" : "ᵘ",
    \ "Renamed"   : "ʳ",
    \ "Unmerged"  : "ᶴ",
    \ "Deleted"   : "ˣ",
    \ "Dirty"     : "˜",
    \ "Clean"     : "ᵅ",
    \ "Unknown"   : "?"
\ }

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

" fzf mappings
nnoremap <c-p> :FZF<CR>

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
