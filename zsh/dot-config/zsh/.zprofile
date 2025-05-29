# set PATH

if [[ "$OSTYPE" == "darwin"* ]]; then
    if type /opt/homebrew/bin/brew &>/dev/null
    then
        eval $(/opt/homebrew/bin/brew shellenv)
        fpath=($(brew --prefix)/share/zsh/site-functions $fpath)
        fpath=($(brew --prefix)/share/zsh-completions $fpath)
        OPENJDKBIN="$HOMEBREW_PREFIX/opt/openjdk/bin"
    fi
    OPENJDKBIN=""
    CYTHON_BIN=""
    export JAVA_HOME=$(/usr/libexec/java_home -v 17)
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    export JAVA_HOME="$(dirname "$(dirname "$(readlink -f "$(which javac)")")")"
    export OPENJDKBIN="$JAVA_HOME/bin"
else
  echo "Unknown OS: $OSTYPE"
fi

LOCALBIN="$HOME/.local/bin"
KREW_BIN="${KREW_ROOT:-$HOME/.krew}/bin"
CYTHON_BIN="~/.local/bin/cython"

export PATH=$LOCALBIN:$PATH:$OPENJDKBIN:$KREW_BIN:$CYTHON_BIN:$GOBIN

# source plugins
source $ZDOTDIR/plugins/fzf-git.sh/fzf-git.sh
source $ZDOTDIR/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $ZDOTDIR/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source $ZDOTDIR/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source $ZDOTDIR/plugins/zsh-async/async.zsh

# fzf
source $ZDOTDIR/plugins/fzf/shell/completion.zsh
source $ZDOTDIR/plugins/fzf/shell/key-bindings.zsh

# custom completions
fpath=(
    $ZDOTDIR/completions
    /usr/local/share/zsh/site-functions
    $HOMEBREW_PREFIX/share/zsh/site-functions
    $fpath
)

# zoxide
eval "$(zoxide init zsh)"

# mise
eval "$(mise activate zsh)"

# atuin
export ATUIN_NOBIND="true"
eval "$(atuin init zsh)"

typeset -U path PATH
typeset -U fpath

# for docker/colima
export DOCKER_HOST="unix://${HOME}/.colima/default/docker.sock"

export RIPGREP_CONFIG_PATH=$XDG_CONFIG_HOME/ripgrep/ripgreprc
export VIRTUAL_ENV_DISABLE_PROMPT=1
export PYTHON3_VENV=$PYTHON3_VENV_BIN/python3
export PIP_REQUIRE_VIRTUALENV=1

export FZF_DEFAULT_COMMAND="fd --type f --hidden --absolute-path"
export FZF_DEFAULT_OPTS=--color="bg:-1,bg+:-1,fg:red,fg+:white,border:black,spinner:0,hl:yellow,header:blue,info:green,pointer:red,marker:blue,prompt:white,hl+:red"

export EZA_PARAMS=('--git' '--group' '--group-directories-first' '--time-style=long-iso' '--color-scale=all' '--icons')

# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

# Advanced customization of fzf options via _fzf_comprun function
# - The first argument to the function is the name of the command.
# - You should make sure to pass the rest of the arguments to fzf.
_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf --preview 'tree -C {} | head -200'   "$@" ;;
    export|unset) fzf --preview "eval 'echo \$'{}"         "$@" ;;
    ssh)          fzf --preview 'dig {}'                   "$@" ;;
    *)            fzf --preview 'bat -n --color=always {}' "$@" ;;
  esac
}

# \e[38;5;Psm where Ps is a color number, or \e[38;2;Pr;Pg;Pbm where Pr, Pg, Pb are RGB values. These can be combined with other attributes, e.g. \e38;5;61;1m or \e38;2;95;95;175;1m for bold slate blue text.
LS_COLORS="rs=0:di=38;5;67:ln=38;5;66:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=31;46:ow=30;46:st=37;44:ex=38;5;101:*.tar=01;33:*.tgz=01;33:*.arc=01;33:*.arj=01;33:*.taz=01;33:*.lha=01;33:*.lz4=01;33:*.lzh=01;33:*.lzma=01;33:*.tlz=01;33:*.txz=01;33:*.tzo=01;33:*.t7z=01;33:*.zip=01;33:*.z=01;33:*.Z=01;33:*.dz=01;33:*.gz=01;33:*.lrz=01;33:*.lz=01;33:*.lzo=01;33:*.xz=01;33:*.bz2=01;33:*.bz=01;33:*.tbz=01;33:*.tbz2=01;33:*.tz=01;33:*.deb=01;33:*.rpm=01;33:*.jar=01;33:*.war=01;33:*.ear=01;33:*.sar=01;33:*.rar=01;33:*.alz=01;33:*.ace=01;33:*.zoo=01;33:*.cpio=01;33:*.7z=01;33:*.rz=01;33:*.cab=01;33:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:"
export LS_COLORS

GREP_COLORS="fn=38;5;65:mt=38;5;67"
export GREP_COLORS
