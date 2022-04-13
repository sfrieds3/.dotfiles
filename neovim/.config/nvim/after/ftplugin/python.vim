setlocal shiftwidth=4 softtabstop=4 foldmethod=indent nofoldenable

function! PythonFuncGrep() abort
    exec("vimgrep /def /j %")
    exec("copen")
endfunction

if has('python')
    setlocal omnifunc=pythoncomplete#Complete
endif
" open quickfix with list of functions
nnoremap <buffer> <silent> _F call PythonFuncGrep()

function! python#Format() abort
    let fp = exists("g:py_formatprg") ? g:py_formatprg : 'yapf --line-length=88'
    let lst = v:lnum + v:count - 1
    silent execute v:lnum . ',' . lst . '!' . fp
endfunction
setlocal formatexpr=python#Format()

if exists("g:py_makeprg")
    " e.g. let g:py_makeprg='pycodestyle\ --ignore=E501\ --format=pylint'
    execute "setlocal makeprg=" . g:py_makeprg
else
    setlocal makeprg='autopep8\ --ignore=E501,E261,E262,E265,E266,W504\ --format=pylint'
endif

setlocal keywordprg=pydoc3
setlocal suffixesadd=.py
setlocal include="from\|import"
setlocal define="^\s*#\s*(def|class)\s+"

" pylint
"let &b:errorformat=%A%f:%l:\ %m,%C,%Z%m
" pep8/pycodestyle
setlocal errorformat=%f:%l:\ [%t%n]\ %m

let g:python_highlight_space_errors = 0

function! InsertPdOptionContext()
    call append(line('.'), "with pd.option_context('display.max_rows', None, 'display.max_columns', None):")
    normal! 2j<Tab>
endfunction

" undo changes
let b:undo_ftplugin = "setlocal sw< sts< fdm< fen< ofu< sua< inc< mp< efm< def< fex< kp<"
