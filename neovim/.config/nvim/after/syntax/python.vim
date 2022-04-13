function! s:Enabled(name)
    return exists(a:name) && {a:name}
endfunction

let g:python_highlight_function_calls = 1

if s:Enabled(g:python_highlight_function_calls)
    syn match pythonFunctionCall '\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*\ze\%(\s*(\)'
    hi link pythonFunctionCall Tag
  
    " highlight function keyword args
    "syn region pythonFCall matchgroup=pythonFName start='[[:alpha:]_]\i*\s*(' end=')' contains=pythonFCall,pythonFCallKeyword
    "syn match pythonFCallKeyword /\i*\ze\s*=[^=]/ contained
    "hi link pythonFCallKeyword Label
    "hi link pythonFName Tag
endif

