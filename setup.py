import os
import subprocess

# symlink files
bash_files = ['bashrc', 'bash_aliases', 'ctags', 'dircolors', 'gitignore', 'inputrc', 'vimrc']

for x in bash_files:
        if not os.path.islink('/home/scott/.' + x):
                os.symlink('/home/scott/.dotfiles/' + x, '/home/scott/.' + x)

if not os.path.islink('/home/scott/.config/nvim/init.vim'):
        os.symlink('/home/scott/.dotfiles', '/home/scott/.congif/nvim/init.vim')

subprocess.call('/home/scott/.dotfiles/setup.sh')