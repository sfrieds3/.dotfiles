SPACESHIP_EXIT_CODE_SHOW=true
SPACESHIP_KUBECTL_SHOW="always"
SPACESHIP_TIME_SHOW=true

SPACESHIP_DIR_TRUNC_REPO=true

SPACESHIP_BATTERY_THRESHOLD=20

SPACESHIP_TIME_FORMAT='%D{%Y-%m-%d %H:%M:%S.%.}'

SPACESHIP_PROMPT_ORDER=(
  user           # Username section
  dir            # Current directory section
  host           # Hostname section
  git            # Git section (git_branch + git_status)
  hg             # Mercurial section (hg_branch  + hg_status)
  package        # Package version
  docker         # Docker section
  docker_compose # Docker section
  kubectl        # Kubectl context section
  python         # Python section
  venv           # virtualenv section
  conda          # conda virtualenv section
  node           # Node.js section
  bun            # Bun section
  deno           # Deno section
  ruby           # Ruby section
  elm            # Elm section
  elixir         # Elixir section
  xcode          # Xcode section
  swift          # Swift section
  golang         # Go section
  perl           # Perl section
  php            # PHP section
  rust           # Rust section
  haskell        # Haskell Stack section
  scala          # Scala section
  kotlin         # Kotlin section
  java           # Java section
  lua            # Lua section
  dart           # Dart section
  julia          # Julia section
  crystal        # Crystal section
  aws            # Amazon Web Services section
  gcloud         # Google Cloud Platform section
  azure          # Azure section
  dotnet         # .NET section
  ocaml          # OCaml section
  vlang          # V section
  zig            # Zig section
  purescript     # PureScript section
  erlang         # Erlang section
  gleam          # Gleam section
  ansible        # Ansible section
  terraform      # Terraform workspace section
  pulumi         # Pulumi stack section
  ibmcloud       # IBM Cloud section
  nix_shell      # Nix shell
  gnu_screen     # GNU Screen section
  exec_time      # Execution time
  async          # Async jobs indicator
  line_sep       # Line break
  battery        # Battery level and status
  jobs           # Background jobs indicator
  exit_code      # Exit code section
  sudo           # Sudo indicator
  char           # Prompt character
)

SPACESHIP_RPROMPT_ORDER=(
    battery
    time
)
