" Autoloaded function for vertical_region.vim maps
function! vertical_region#(count, up, mode) abort

  " Reselect any selection
  if a:mode ==# 'x'
    normal! gv
  endif

  " Get line and column number
  let num = line('.')
  let col = col('.')

  " Move up or down through buffer, counting hits as we go
  let hits = 0
  while a:up ? num > 1 : num < line('$')

    " Increment or decrement line number
    let num += a:up ? -1 : 1

    " If the line has any non-space characters up to the current column, we
    " have a hit; break the loop as soon as we have the count we need
    let line = getline(num)
    if strpart(line, 0, col) =~# '\S'
      let hits += 1
      if hits == a:count
        break
      endif
    endif

  endwhile

  " If not moving linewise for operator mode and not in first column, move to
  " same column after line jump; is there a way to do this in one jump?
  let keys = num . 'G0'
  if a:mode ==# 'o'
    let keys = 'V' . keys
  elseif col > 1
    let keys .= col - 1 . 'l'
  endif

  " Run normal mode commands
  execute 'normal! ' . keys

endfunction
