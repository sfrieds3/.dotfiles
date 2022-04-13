function! git#current_branch()
    let s:g_branch = system('git rev-parse --abbrev-ref HEAD')
    echo s:g_branch
endfunction!

command! GitBranch call git#current_branch()
