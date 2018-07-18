#! /usr/bin/python3
import os
import subprocess
from os.path import expanduser

user_home = expanduser("~")

# symlink bash, ctags, gitignore, vimrc
bash_files = ['bashrc', 'bash_aliases', 'ctags', 'dircolors', 'gitignore', 'inputrc', 'vimrc']

for x in bash_files:
        if not os.path.islink(user_home + '/.' + x):
                os.symlink(user_home + '/.dotfiles/' + x, user_home + '/.' + x)

if not os.path.islink(user_home + '/.config/nvim/init.vim'):
        os.symlink(user_home + '/.dotfiles', user_home + '/.congif/nvim/init.vim')

# I'm lazy, shell script to set up to set custom colors for git
subprocess.call(user_home + '/.dotfiles/setup.sh')

# I'm still lazy.. shell script to apt install neovim etc etc
print(user_home + '/.dotfiles/apt_install.sh')
subprocess.call(user_home + '/.dotfiles/apt_install.sh')

# install golang

