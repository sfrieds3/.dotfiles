# zmodload zsh/zprof

() {
  local file=

  # For JetBrains IDEs: only load env files, skip interactive setup (zle, compdef, etc.)
  if [[ -n "$INTELLIJ_ENVIRONMENT_READER" ]]; then
    for file in $ZDOTDIR/rc.d/0[01]-*.zsh $ZDOTDIR/rc.d/12-*.zsh(N); do
      . $file
    done
    return 0
  fi

  # Load all of the files in rc.d that start with <number>- and end in `.zsh`.
  # (n) sorts the results in numerical order.
  # <0->  matches any integer >= 0.
  #  <->  is the same as <0->
  # <0-9> matches any integer >= 0 and <= 9.
  #  <-9> is the same as <0-9>
  # See https://zsh.sourceforge.io/Doc/Release/Expansion.html#Glob-Operators
  for file in $ZDOTDIR/rc.d/<->-*.zsh(n); do
    . $file
  done
} "$@"

# zprof
