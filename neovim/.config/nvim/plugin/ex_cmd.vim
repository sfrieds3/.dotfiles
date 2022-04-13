" do what I want when I make typo in ex cmd
" taken from sanctum.geek.nz
if exists('g:loaded_ex_cmd') || &compatible
  finish
endif
if !has('user_commands') || v:version < 600
  finish
endif
let g:loaded_ex_cmd = 1

command -bang -bar -complete=file -nargs=? E edit<bang> <args>
command -bang -bar -complete=file -nargs=? W write<bang> <args>
command -bang -bar -complete=file -nargs=? WQ wq<bang> <args>
command -bang -bar -complete=file -nargs=? Wq wq<bang> <args>
command -bang -bar Q quit<bang>
command -bang -bar Qa qall<bang>
command -bang -bar QA qall<bang>
command -bang -bar Wa wall<bang>
command -bang -bar WA wa<bang>
