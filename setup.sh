#!/bin/bash

codedir=$HOME/code
vimdir=$HOME/.vim
datetime="`date +%Y%m%d%H%M%S`"
dotfiles=$HOME/.dotfiles

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

# vim as editor
git config --global core.editor vim

# core excludes file
git config --global core.excludesfile ~/.gitignore


# directory colors for ls
LS_COLORS='ow=01;36;40'
export LS_COLORS

# apt install packages for ubuntu
sudo apt update
sudo apt install -y exuberant-ctags
sudo apt install -y leiningen
sudo apt install -y postgresql
sudo apt install -y tmux
sudo apt install -y htop
sudo apt install -y mysql-server
sudo apt install -y libmysqlclient-dev
sudo apt install -y rvm
sudo apt install -y meld
sudo apt install -y kdiff3
sudo apt install -y gitk
sudo apt install -y rawtherapee
sudo apt install -y vim-nox
sudo apt install -y libxml2-utils
sudo apt install -y dconf-tools
sudo apt install -y sbcl
sudo apt install -y clisp
sudo apt install -y gnome-tweaks
sudo apt install -y git
sudo apt install -y curl
sudo apt install -y cmake
sudo apt install -y fonts-font-awesome
sudo apt install -y libreoffice
sudo apt install -y mutt
sudo apt install -y chicken-bin
sudo apt install -y guile-3.0
sudo apt install -y net-tools
sudo apt install -y ufw
sudo apt install -y openssh-server
sudo apt install -y xdg-desktop-portal
sudo apt install -y obs-studio
sudo apt install -y clang
sudo apt install -y python3-pip
sudo apt install -y nnn
sudo apt install -y zsh
sudo apt install -y cinnamon

sudo snap install spotify --classic
sudo snap install datagrip --classsic

# enable firewall
sudo ufw enable
sudo sfw logging on
sudo ufw allow ssh

echo "Checking to see if $codedir is already created"
if ! [ -d "$codedir" ]; then
    echo "it is not... calling mkdir $codedir"
    mkdir $codedir
fi

if [ -d "$vimdir" ]; then
    echo "$vimdir already exists.. moving to $vimdir.$datetime"
    mv $vimdir $vimdir.$datetime
    echo "cloning vimconfig: git clone git@github.com:scwfri/vimconfig $vimdir"
    git clone git@github.com:scwfri/vimconfig $vimdir
fi

for repo in AdventOfCode hackn noteit vim
do
    if ! [ -d "$codedir/$repo" ]; then
        echo "cloning: git clone git@github.com:scwfri/$repo $codedir/$repo"
        git clone git@github.com:scwfri/$repo $codedir/$repo
    fi
done

for file in tmux.conf bashrc bash_aliases inputrc csirc zshrc gitignore
do 
    if [ -f "$HOME/.$file" ]; then
        echo "$HOME/$file already exists.. moving to $HOME/.$file.$datetime"
    fi

    echo "running: ln -s $dotfiles/$file $HOME/.$file..."
    ln -s $dotfiles/$file $HOME/.$file
done

# python pip installation
/usr/bin/python3 -m pip install --user autopep8
/usr/bin/python3 -m pip install --user pylint
/usr/bin/python3 -m pip install --user black
/usr/bin/python3 -m pip install --user yapf

# install sbt
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt-get update
sudo apt-get install sbt

chicken-install linenoise -s

# install pure for zsh
mkdir -p "$HOME/.zsh"
git clone https://github.com/sindresorhus/pure.git "$HOME/.zsh/pure"

chsh -s /bin/zsh

source ~/.bashrc
