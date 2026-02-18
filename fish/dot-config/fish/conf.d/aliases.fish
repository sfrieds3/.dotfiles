alias hupdate 'history merge'

alias d 'dirs -v'
for i in (seq 0 9)
    alias $i "cd +$i"
end

alias kubectl kubecolor

alias l "eza $EZA_PARAMS"
alias lg "eza --git-ignore $EZA_PARAMS"
alias ll "eza --all --header --long $EZA_PARAMS"
alias llm "eza --all --header --long --sort=modified $EZA_PARAMS"
alias la "eza -lbhHigUmuSa"
alias lx "eza -lbhHigUmuSa@"
alias lt "eza --tree $EZA_PARAMS"
alias tree "eza --tree $EZA_PARAMS --git-ignore"
alias treea "eza --tree $EZA_PARAMS"

alias k9s 'k9s --readonly'
