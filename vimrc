" start of plugins {{{

" install plugged if not installed
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged') " call plugged to manage plugins"

Plug 'neovim/python-client' " required for nvim python plugins
Plug 'airblade/vim-gitgutter' "show git diff in gutter
Plug 'easymotion/vim-easymotion' " vim easymotion
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
Plug 'ap/vim-css-color' " show CSS colors inline
Plug 'mbbill/undotree' " visual undo tree
Plug 'ludovicchabant/vim-gutentags' " tags management
Plug 'unblevable/quick-scope' " highlight next occurrence of letters

Plug 'JBakamovic/yavide' " c/c++
Plug 'vim-ruby/vim-ruby' " ruby
Plug 'fatih/vim-go' " golang
"
" fzf- fuzzy file finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

"-------------------------------------------------------"

" colors
Plug 'sfrieds3/dim.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'ayu-theme/ayu-vim'
Plug 'morhetz/gruvbox'
Plug 'NLKNguyen/papercolor-theme'
Plug 'sfrieds3/vim-hickop-colors'
Plug 'sjl/badwolf'

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
Plug 'davidhalter/jedi-vim' " python autocomplete
Plug 'zchee/deoplete-jedi' " python autocomplete
Plug 'mdempsky/gocode', { 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh' }
Plug 'zchee/deoplete-go', { 'do': 'make'} " golang autocomplete
Plug 'zchee/deoplete-clang' " clang deoplete backend

"" Scala stuff
Plug 'ensime/ensime-vim', { 'do': ':UpdateRemotePlugins' } " Scala
Plug 'derekwyatt/vim-scala'

" ALL PLUGINS BEFORE THIS LINE
call plug#end()

"}}}

" theme settings {{{

" Color settings!
if !exists("g:syntax_on")
  syntax enable
endif

"set colorscheme below
colorscheme dim

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
  highlight MatchParen guifg=white guibg=grey50
  highlight Title guifg=#aaaaaa
  highlight PreProc guifg=#aaaaaa
endif

" nord colorscheme settings
if g:colors_name == 'nord'
  let g:nord_italic = 1
  let g:nord_italic_comments = 1
  highlight Comment guifg=#D08770 " comment colors
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

" end statusline

" }}}

" basic vim settings {{{

" update leader
let mapleader = ","
" map local leader- can use for other commands with <localleader>
let maplocalleader = "\\"

"wrapped lines go down/up to next row
noremap j gj
noremap k gk

" retain buffers until quit
set hidden

" use modeline
set modeline

" No bells!
set visualbell

" Fast scrolling
set ttyfast

" set cursorline for current window only
augroup CursorLine
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

" allow recursive searching with :find
set path+=**

" not case sensitive, unless all caps
set ignorecase
set smartcase

" set characters for end of line & tab
"set list
set showbreak=↪
set listchars=tab:\|_,nbsp:␣,extends:…,precedes:…

" make backspace work
set backspace=2

" show filename in window titlebar
set title

" set lines above/below cursor
set so=0

" turn on wild menu
set wildmenu

"2nd tab: complete first alternative, allow tab/S-tab to cycle back and forth
set wildmode=longest:full,full
set foldcolumn=0

" path/file expansion in colon-mode
set wildmode=longest:full,list:full,list:longest
set wildchar=<TAB>

" do not highlight in insert mode
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
augroup hiwhitespace
  autocmd!
  autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
  autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
  autocmd InsertLeave * match ExtraWhitespace /\s\+$/
  autocmd BufWinLeave * call clearmatches()
augroup END

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
augroup resize
  autocmd!
  autocmd VimResized * :wincmd =
augroup END

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

set maxmempattern=20000 " increase max memory -- show syntax highlighting for large files
set history=1000
set undofile
set undodir=~/.vim/undo " where to save undo history
set undolevels=1000 " How many undos
set undoreload=10000 " number of lines to save for undo
set backupdir=~/.vim/backup
set viminfo='1000 " need for FzfHistory
set noswapfile

set nocompatible
set foldmethod=marker
set clipboard=unnamed "use system default clipboard

" Update term title
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

"}}}

" neovim settings {{{

if has('nvim')
  " neovim commands
  " neovim terminal - esc to exit terminal-mode
  :tnoremap <Esc> <C-\><C-n>

  " set termguicolors
  set termguicolors

  " do not show line numbers in terminal mode
  augroup termsettings
    autocmd!
    autocmd TermOpen * setlocal nonumber norelativenumber
  augroup END

endif

" }}}

" general language settings {{{

augroup lang
  autocmd!

  autocmd FileType html setlocal shiftwidth=2 tabstop=2 softtabstop=2
  autocmd FileType go setlocal shiftwidth=8 tabstop=8 softtabstop=8
  autocmd FileType c setlocal shiftwidth=8 tabstop=8 softtabstop=8
  autocmd FileType python setlocal shiftwidth=4 tabstop=4 softtabstop=4
  autocmd FileType scala setlocal shiftwidth=2 tabstop=2 softtabstop=2
  autocmd FileType vim setlocal shiftwidth=2 tabstop=2 softtabstop=2
  "autocmd BufRead,BufNewFile *.md setlocal textwidth=80
  "autocmd BufRead,BufNewFile *.html setlocal textwidth=80
augroup END

" golang {{{
" no warning for out of date nvim
let g:go_version_warning = 0

" goimport on save
let g:go_fmt_command = "goimports"
let g:go_metalinter_autosave = 1
"
" show definition when hovering
let g:go_auto_type_info = 1

augroup go
  autocmd!

  autocmd FileType go set nolist

  autocmd FileType go nmap <silent> <localleader>v <Plug>(go-def-vertical)
  autocmd FileType go nmap <silent> <localleader>s <Plug>(go-def-split)
  autocmd FileType go nmap <silent> <localleader>d <Plug>(go-def-tab)

  autocmd FileType go nmap <silent> <localleader>x <Plug>(go-doc-vertical)

  autocmd FileType go nmap <silent> <localleader>i <Plug>(go-info)
  autocmd FileType go nmap <silent> <localleader>l <Plug>(go-metalinter)

  autocmd FileType go nmap <silent> <localleader>b :<C-u>call <SID>build_go_files()<CR>
  autocmd FileType go nmap <silent> <localleader><localleader>t  <Plug>(go-test)
  autocmd FileType go nmap <silent> <localleader>r  <Plug>(go-run)
  autocmd FileType go nmap <silent> <localleader>e  <Plug>(go-install)

  autocmd FileType go nmap <silent> <localleader>c <Plug>(go-coverage-toggle)
augroup END
" }}}

" python {{{
" python-mode
let g:pymode_options_colorcolumn = 0
let g:pymode_folding = 0
let g:pymode_options = 0
" }}}

" Scala {{{
augroup scala
  autocmd!
  autocmd BufWritePost *.scala silent :EnTypeCheck
  autocmd FileType scala nnoremap <localleader>k :EnType<CR>
  autocmd FileType scala nnoremap <localleader>df :EnDeclaration<CR>
augroup END

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
" }}}

" Markdown {{{
augroup markdown
  autocmd!

  autocmd FileType markdown let b:deoplete_disable_auto_complete = 1
augroup END

" }}}

" }}}

" remapping key commands {{{

" open python repl
nnoremap<localleader>P :terminal python3<cr> :keepalt file *python*<cr>

" open fish shell
nnoremap <localleader>f :terminal fish<cr>

" show list of digraphs -- special symbols
nnoremap <localleader>D :help digraphs<cr>:175<cr>

" upper case last word using ctrl+u
inoremap <C-u> <esc>mzgUiw`za

" Shift-Tab enters actual tab
inoremap <S-Tab> <C-V><Tab>

" remap 0 to first nonblank character
nnoremap 0 ^

" stay where you are on * from fatih (http://www.github.com/fatih/dotfiles)
nnoremap <silent> * :let stay_star_view = winsaveview()<cr>*:call winrestview(stay_star_view)<cr>

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

" sudo for write
cmap w!! w !sudo tee % >/dev/null

" toggle line numbers
nnoremap <silent> <Leader>n :set invnumber<CR>

" open Ack quick fix window to show TODO's
nnoremap <silent> <leader>vt :Ack! TODO<CR>

" open Ack quick fix winow to show current word
nnoremap <leader>A :Ack! <cword><CR>

" ack for a word
nnoremap <leader>a :Ack!

" show avilable marks and be ready to swtich - now use fzf
"nnoremap <leader>mm :<C-u>marks<CR>:normal! `

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

"}}}

" moving around, tabs, windows and buffers {{{

" Smart way to move between windows
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l
nnoremap <leader>ww <C-w>w
nmap <leader>w <C-W>

" go to previous and next tab/buffers
nnoremap <leader>bp :bprevious<cr>
nnoremap <leaderbn :bnext<cr>
nnoremap <M-<> :bprevious<cr>
nnoremap <M->> :bnext<cr>
nnoremap <M-h> :tabprevious<cr>
nnoremap <M-l> :tabnext<cr>

" resize splits
nnoremap <C-M-h> <C-w><
nnoremap <C-M-j> <C-w>+
nnoremap <C-M-k> <C-w>-
nnoremap <C-M-l> <C-w>>

" Close the current buffer
nnoremap <leader>bd :bdelete<cr>


" Useful mappings for managing tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>th :-tabmove<cr>
nnoremap <leader>tl :+tabmove<cr>

" rename tab
nnoremap <leader>tr :TabooRename<space>
nnoremap <leader>T :TabooOpen<space>

"}}}

" plugin configurations {{{

" ale {{{
let g:ale_lint_on_enter = 1
" }}}

" easymotion {{{
highlight link EasyMotionTarget Todo
" }}}

" deoplete {{{
let g:deoplete#enable_at_startup = 1
"autocmd CompleteDone * pclose!
let g:deoplete#omni_patterns = {}
let g:deoplete#omni_patterns.scala = '[^. *\t]\.\w*\|: [A-Z]\w*'

" make enter work with deoplete in insert mode
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function() abort
  return deoplete#close_popup() . "\<CR>"
endfunction
" }}}

" quickscope {{{
" Trigger a highlight in the appropriate direction when pressing these keys:
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
" }}}

" fzf {{{
"
" Fzf prefix
let g:fzf_command_prefix = 'Fzf'

" fzf through project
nnoremap <A-p> :FzfFiles<CR>

" fzf through file
nnoremap <leader>ll :FzfLines<CR>

" recent files
nnoremap <leader>ff :FzfHistory<CR>

" marks
nnoremap <leader>mm :FzfMarks<CR>

" tags
nnoremap <leader><space> :FzfTags<cr>

" fzf through buffers
function! s:buflist()
  redir => ls
  silent ls
  redir END
  return split(ls, '\n')
endfunction

function! s:bufopen(e)
  execute 'buffer' matchstr(a:e, '^[ 0-9]*')
endfunction

nnoremap <silent> <Leader>bb :call fzf#run({
      \   'source':  reverse(<sid>buflist()),
      \   'sink':    function('<sid>bufopen'),
      \   'options': '+m',
      \   'down':    len(<sid>buflist()) + 2
      \ })<CR>
" }}}

" Git gutter {{{
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
" }}}

" fugitive {{{
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>GP :Gpush<CR>
nnoremap <Leader>gb :Gblame
nnoremap <Leader>gd :Gdiff<CR>
" }}}

" vim-over {{{
nnoremap <leader>vr :OverCommandLine<CR>%s/
" }}}

" NERDTree {{{
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
" }}}

" netrw {{{
" ,O opens directory in netrw
nnoremap <Leader>O :Explore %:h<cr>
" }}}

" ag/ack {{{
" use ag for ack search, fall back on ack if ag not avail
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" ack/ag with <leader> a
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ack!<Space>

" use ag as default ack client
let g:ackprg = 'ag --nogroup --nocolor --column'
" }}}

" tagbar {{{
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
" }}}

" }}}

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

" notes {{{

" :helpgrep <text> - grep for <text> in all help docs
" :cn :cp to go to next or previous result from :helpgrep
" Tags:
" - Use ^] to jump to tag under cursor
" - Use g^] for ambiguous tags
" - Use ^t to jump back up the tag stack

"}}}
