#! /usr/bin/python3
import os
import subprocess

# symlink bash, ctags, gitignore, vimrc
bash_files = ['bashrc', 'bash_aliases', 'ctags', 'dircolors', 'gitignore', 'inputrc', 'vimrc']

for x in bash_files:
        if not os.path.islink('/home/scott/.' + x):
                os.symlink('/home/scott/.dotfiles/' + x, '/home/scott/.' + x)

if not os.path.islink('/home/scott/.config/nvim/init.vim'):
        os.symlink('/home/scott/.dotfiles', '/home/scott/.congif/nvim/init.vim')

# I'm lazy, shell script to set up to set custom colors for git
subprocess.call('/home/scott/.dotfiles/setup.sh')

# install and set up tmux

# install neovim

# vim pre-req's

# install spotify

# install dropbox

# install vs code

# install simplenote

# install wireshark
