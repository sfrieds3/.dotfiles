fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}


# Path Management
# source: https://github.com/camspiers/dotfiles/blob/master/files/.bashrc

################################################################################
# Joins paths together by ":"
# Arguments:
#     $1: Array of paths, e.g. join ARR[@]
# Returns:
#     string
################################################################################
join() { a=("${!1}"); local IFS=":"; echo "${a[*]}"; }

################################################################################
# Deduplicates paths separated by ":"
# Arguments:
#     $1: string of paths separated by ":"
# Returns:
#     string
################################################################################
dedup() { echo -n $1 | awk -v RS=: -v ORS=: '!arr[$0]++'; }
