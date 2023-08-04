set -g VIRTUALFISH_HOME $HOME/.venv
set -g VIRTUALFISH_VERSION 2.5.5
set -g VIRTUALFISH_PYTHON_EXEC $HOME/.local/pipx/venvs/virtualfish/bin/python
source $HOME/.local/pipx/venvs/virtualfish/lib/python3.11/site-packages/virtualfish/virtual.fish
source $HOME/.local/pipx/venvs/virtualfish/lib/python3.11/site-packages/virtualfish/compat_aliases.fish
emit virtualfish_did_setup_plugins
