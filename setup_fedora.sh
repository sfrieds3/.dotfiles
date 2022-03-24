#!/bin/bash
# to change display resolution: xrandr --output <display> --mode <resolution>

codedir=$HOME/dev
vimdir=$HOME/.vim
datetime="`date +%Y%m%d%H%M%S`"
dotfiles=$HOME/.dotfiles

# add rpmfusion, so we can install ffmpeg
# free
#sudo dnf install \
#  https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm

# non-free
#sudo dnf install \
#  https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm

dnf_install=(
    neovim git ripgrep fd-find bat fzf gitk clang clang-tools clang-tools-extra
    htop ShellCheck llvm libgcrypt-devel git-core automake binutils-devel curl
    cmake mpfr-devel libmpc-devel gmp-devel e2f sprogs ninja-build patch ccache
    rsync @"C Development Tools and Libraries" @Virtualization git-core zlib
    zlib-devel gcc-c++ patch readline readline-devel libyaml-devel libffi-devel
    openssl-devel make bzip2 autoconf automake libtool bison curl sqlite-devel
    perl-core guile chicken gcc lua java-11-openjdk-devel texinfo ruby-devel
    gimp nasm xorriso
)

# python build deps
sudo dnf builddep python3

for i in "${dnf_install[@]}"
do
    sudo dnf install -y $i
done

flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install -y --noninteractive flathub com.spotify.Client
flatpak install -y --noninteractive flathub org.videolan.VLC
flatpak install -y --noninteractive flathub com.obsproject.Studio
flatpak install -y --noninteractive flathub us.zoom.Zoom

# enable ssh
# https://docs.fedoraproject.org/en-US/fedora/rawhide/system-administrators-guide/infrastructure-services/OpenSSH/#s2-ssh-configuration-sshd
#sudo systemctl enable sshd.service
#
#sudo systemctl disable telnet.service
#sudo systemctl disable rsh.service
#sudo systemctl disable rlogin.service
#sudo systemctl disable vsftpd.service

#echo "Checking to see if $codedir is already created"
#if ! [ -d "$codedir" ]; then
#    echo "it is not... calling mkdir $codedir"
#    mkdir $codedir
#fi
#
#if [ -d "$vimdir" ]; then
#    echo "$vimdir already exists.. moving to $vimdir.$datetime"
#    mv $vimdir $vimdir.$datetime
#    echo "cloning vimconfig: git clone git@github.com:scwfri/vimconfig $vimdir"
#    git clone git@github.com:scwfri/vimconfig $vimdir
#fi
#
#for file in tmux.conf bashrc csirc gitignore tmux.statusline gitconfig zshenv ctags sbclrc inputrc ripgreprc
#do
#    if [ -f "$HOME/.$file" ]; then
#        echo "$HOME/$file already exists.. moving to $HOME/.$file.$datetime"
#    fi
#
#    echo "running: ln -s $dotfiles/$file $HOME/.$file..."
#    ln -s $dotfiles/$file $HOME/.$file
#done

#if ! [ -d "$XDG_CONFIG_HOME/lib-scwfri" ]; then
#    echo "ln -s $HOME/.dotfiles/lib-scwfri $XDG_CONFIG_HOME/lib-scwfri"
#    ln -s $HOME/.dotfiles/lib-scwfri $XDG_CONFIG_HOME
#fi

#if ! [ -d "$XDG_CONFIG_HOME/zsh" ]; then
#    echo "ln -s $HOME/.dotfiles/zsh $XDG_CONFIG_HOME/zsh"
#    ln -s $HOME/.dotfiles/zsh $XDG_CONFIG_HOME
#fi

#if ! [ -d "$XDG_CONFIG_HOME/kitty" ]; then
#    echo "ln -s $HOME/.dotfiles/kitty $XDG_CONFIG_HOME/kitty"
#    ln -s $HOME/.dotfiles/kitty $XDG_CONFIG_HOM/E
#fi

# python pip installation
/usr/bin/python3 -m ensurepip
/usr/bin/python3 -m pip install --upgrade pip
/usr/bin/python3 -m pip install --user 'python-lsp-server[all]'
/usr/bin/python3 -m pip install --user autopep8
/usr/bin/python3 -m pip install --user pylint
/usr/bin/python3 -m pip install --user black
/usr/bin/python3 -m pip install --user yapf
/usr/bin/python3 -m pip install --user pycodestyle
/usr/bin/python3 -m pip install --user compiledb
/usr/bin/python3 -m pip install --user pre-commit
/usr/bin/python3 -m pip install --user virtualenv

# install nvm
wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash

# install python pyright language server
npm install -g pyright

# install rbenv and ruby-build
git clone https://github.com/rbenv/rbenv.git ~/.rbenv
mkdir -p "$(rbenv root)"/plugins
git clone https://github.com/rbenv/ruby-build.git "$(rbenv root)"/plugins/ruby-build

# install sbt
#curl https://bintray.com/sbt/rpm/rpm | sudo tee /etc/yum.repos.d/bintray-sbt-rpm.repo
#sudo dnf install sbt

# install linenoise for chicken
#CSC_OPTIONS='-I/usr/include/chicken' chicken-install linenoise -s

# install quicklisp
#curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
#sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
#       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
#       --eval '(ql:add-to-init-file)' \
#       --quit
#
