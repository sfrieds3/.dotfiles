#!/bin/bash

# update git color for untracked files to white
git config --global color.status.untracked white
git config --global color.status.changed "white normal dim"
git config --global color.status.nobranch "red bold ul"

# update git color for diff
git config --global color.diff.old "yellow reverse dim"

# add difftool
git config --global diff.guitool meld
git config --global diff.tool vimdiff
git config --global difftool.prompt false

# emacs as editor
git config --global core.editor vim

# directory colors for ls
LS_COLORS='ow=01;36;40'
export LS_COLORS

# go get -u github.com/jstemmer/gotags
# go get -u github.com/nsf/gocode
# go get github.com/rogpeppe/godef
# go get -u github.com/derekparker/delve/cmd/dlv
# go get -u github.com/sourcegraph/go-langserver
# gometalinter --install
# export PATH=$PATH:$(go env GOPATH)/bin

# cargo install racer

# apt install packages for ubuntu
sudo apt update
sudo apt install -y exuberant-ctags
sudo apt install -y leiningen
sudo apt install -y postgresql
sudo apt install -y tmux
sudo apt install -y mysql-server
sudo apt install -y libmysqlclient-dev
sudo apt install -y rvm
sudo apt install -y meld
sudo apt install -y kdiff3
sudo apt install -y gitk
sudo apt install -y vim-nox
sudo apt install -y libxml2-utils
sudo apt install -y dconf-tools
sudo apt install -y sbcl
sudo apt install -y clisp
sudo apt install -y gnome-tweaks
sudo apt install -y git
sudo apt install -y python3-pip
sudo apt install -y virtualenv
sudo apt install -y virtualenvwrapper
sudo apt install -y curl
#sudo apt install -y libreoffice
sudo snap install emacs
sudo snap install spotify --classic
sudo snap install datagrip --classsic

# python pip installation
python3 -m pip install pip

# virtualenv
source ~/.bashrc
pip3 install --user virtualenvwrapper
mkdir $WORKON_HOME
mkvirtualenv -p python3 venv
workon venv && pip3 install 'python-language-server[all]'

# link all the things, download all the git
mkdir ~/.emacs.d
mkdir ~/code
mkdir ~/code/forked
mkdir ~/.emacs.d/backups
mkdir ~/.emacs.d/auto-save-list
mkdir ~/.emacs.d/savehist

#git clone git@github.com:scwfri/.dotfiles.git ~/.dotfiles
git clone git@github.com:scwfri/AdventOfCode.git ~/code/AdventOfCode
git clone git@github.com:scwfri/hackn.git ~/code/hackn
git clone git@github.com:scwfri/noteit.git ~/code/noteit

#ln -sf ~/.dotfiles/vimrc ~/.vimrc
ln -sf ~/.dotfiles/tmux.conf ~/.tmux.conf
ln -sf ~/.dotfiles/init.el ~/.emacs.d/init.el
ln -sf ~/.dotfiles/bashrc ~/.bashrc
ln -sf ~/.dotfiles/bash_aliases ~/.bash_aliases
ln -sf ~/.dotfiles/gitattributes ~/.gitattributes
ln -sf ~/.dotfiles/inputrc ~/.inputrc
ln -sf ~/.dotfiles/home.el ~/.emacs.d/home.el

source ~/.bashrc
