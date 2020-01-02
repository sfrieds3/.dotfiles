#!/bin/bash

# update git color for untracked files to white
git config --global color.status.untracked white
git config --global color.status.changed "white normal dim"
git config --global color.status.nobranch "red bold ul"

# update git color for diff
git config --global color.diff.old "yellow reverse dim"

# add difftool
git config --global diff.tool meld
git config --global difftool.prompt false

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
sudo apt install exuberant-ctags
sudo apt install leiningen
sudo apt install postgresql
sudo apt install tmux
sudo apt install mysql-server
sudo apt install libmysqlclient-dev
sudo apt install rvm
sudo apt install meld
sudo apt install gitk
sudo apt install vim-nox
sudo apt install libxml2-utils
sudo apt install dconf-tools
sudo apt install sbcl
sudo apt install gnome-tweaks
sudo snap install emacs
sudo snap install spotify --classic

# python pip installation
#python3 -m pip install jedi
#python3 -m pip install websocket-client sexpdata
#python3 -m pip install virtualenv

# add vimrc and tmux.conf
mkdir ~/.emacs.d
ln -s ~/.dotfiles/vimrc ~/.vimrc
ln -s ~/.dotfiles/tmux.conf ~/.tmux.conf
ln -s ~/.dotfiles/init.el ~/.emacs.d/init.el
ln -s ~/.dotfiles/bashrc ~/.bashrc
ln -s ~/.dotfiles/bash_aliases ~/.bash_aliases
ln -s ~/.dotfiles/gitattributes ~/.gitattributes
ln -s ~/.dotfiles/inputrc ~/.inputrc
