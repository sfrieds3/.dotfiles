#! /usr/bin/python3

from os.path import expanduser
import os
import subprocess

USER_HOME = expanduser("~")

# symlink bash, ctags, gitignore, vimrc
BASH_FILES = ['bashrc', 'bash_aliases', 'ctags', 'dircolors', 'gitignore', 'inputrc', 'vimrc']

for x in BASH_FILES:
    if not os.path.islink(USER_HOME + '/.' + x):
        os.symlink(USER_HOME + '/.dotfiles/' + x, USER_HOME + '/.' + x)

if not os.path.islink(USER_HOME + '/.config/nvim/init.vim'):
    os.symlink(USER_HOME + '/.dotfiles', USER_HOME + '/.congif/nvim/init.vim')

# shell script to complete setup
subprocess.call(USER_HOME + '/.dotfiles/setup.sh')
