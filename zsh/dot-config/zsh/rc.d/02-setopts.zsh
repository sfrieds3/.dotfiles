setopt   INTERACTIVE_COMMENTS # allow comments even in interactive shells
unsetopt AUTO_CD              # assume cd if directory passed
unsetopt CORRECT              # try to correct spelling of commands
unsetopt CORRECT_ALL          # try to correct spelling of all arguments
setopt   PROMPT_SUBST         # allow expansions in prompt (needed for __git_ps1)
setopt   COMPLETE_IN_WORD     # allow tab completion in the middle of a word
setopt   AUTO_PUSHD           # Push the current directory visited on the stack.
setopt   PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt   PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
unsetopt FLOW_CONTROL         # disable C-s/C-q behavior
setopt   CHECK_JOBS           # check for running jobs on exit and warn (default behavior)
setopt   NO_HUP               # but do not send HUP signal to bg jobs on exit
setopt   HISTVERIFY           # show history expansions
setopt   IGNORE_EOF           # ignore EOF (but still close after 10 consecutive)
setopt   NO_CLOBBER           # prohibit 'cat foo > bar' if bar exists. use >! instead
setopt   EXTENDED_GLOB        # enables various things, most notably ^negation. '^', '#' and forgotwhich :/ see cheatsheet & http://zsh.dotsrc.org/Intro/intro_2.html#SEC2
setopt   NOTIFY               # [on] this will put info from finished background processes over the current line
setopt   TRANSIENT_RPROMPT    # remove rprompt on enter
