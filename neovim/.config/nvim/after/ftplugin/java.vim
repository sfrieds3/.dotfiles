setlocal makeprg=javac

setlocal errorformat=%A%f:%l:\ %m,%+Z%p^,%+C%.%#,%-G%.%#

let b:undo_ftplugin = "setlocal efm< mp<"
