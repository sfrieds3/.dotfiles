#!/bin/bash

codedir=$HOME/code
vimdir=$HOME/.vim
datetime="`date +%Y%m%d%H%M%S`"
dotfiles=$HOME/.dotfiles

# add rpmfusion, so we can install ffmpeg
# free
sudo dnf install \
  https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm

# non-free
sudo dnf install \
  https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm

sudo dnf install -y @cinnamon-desktop
sudo dnf install -y chicken
sudo dnf install -y redhat-rpm-config
sudo dnf install -y nnn
sudo dnf install -y guile
sudo dnf install -y htop
sudo dnf install -y sbcl
sudo dnf install -y gitk
#sudo dnf install -y java-devel
sudo dnf install -y java-11-openjdk-devel
sudo dnf install -y dconf-editor
sudo dnf install -y clisp
sudo dnf install -y kdiff3
sudo dnf install -y meld
sudo dnf install -y clang
sudo dnf install -y libxml2
sudo dnf install -y ffmpeg
sudo dnf install -y openssh-server
sudo dnf install -y postgresql
sudo dnf install -y postgresql-server

# cinnamon installs vim-powerline, which we dont want
# which then removes vim-enhanced... ugh
sudo dnf remove -y vim-powerline
sudo dnf install -y vim-enhanced

flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install -y --noninteractive flathub com.spotify.Client
flatpak install -y --noninteractive flathub org.videolan.VLC
flatpak install -y --noninteractive flathub com.obsproject.Studio
flatpak install -y --noninteractive flathub us.zoom.Zoom
flatpak install -y --noninteractive flathub com.jetbrains.IntelliJ-IDEA-Ultimate
flatpak install -y --noninteractive flathub com.jetbrains.DataGrip

# enable ssh
# https://docs.fedoraproject.org/en-US/fedora/rawhide/system-administrators-guide/infrastructure-services/OpenSSH/#s2-ssh-configuration-sshd
sudo systemctl enable sshd.service

sudo systemctl disable telnet.service
sudo systemctl disable rsh.service
sudo systemctl disable rlogin.service
sudo systemctl disable vsftpd.service

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

for file in tmux.conf bashrc csirc gitignore tmux.statusline gitconfig
do
    if [ -f "$HOME/.$file" ]; then
        echo "$HOME/$file already exists.. moving to $HOME/.$file.$datetime"
    fi

    echo "running: ln -s $dotfiles/$file $HOME/.$file..."
    ln -s $dotfiles/$file $HOME/.$file
done

if ! [ -d "$HOME/bin" ]; then
    echo "ln -s $home/.dotfiles/bin $home/bin"
    ln -s $home/.dotfiles/bin $home/bin
fi

# python pip installation
/usr/bin/python3 -m pip install --user autopep8
/usr/bin/python3 -m pip install --user pylint
/usr/bin/python3 -m pip install --user black
/usr/bin/python3 -m pip install --user yapf
/usr/bin/python3 -m pip install --user pycodestyle

# install sbt
curl https://bintray.com/sbt/rpm/rpm | sudo tee /etc/yum.repos.d/bintray-sbt-rpm.repo
sudo dnf install sbt

# install linenoise for chicken
CSC_OPTIONS='-I/usr/include/chicken' chicken-install linenoise -s

# install quicklisp
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
       --eval '(ql:add-to-init-file)' \
       --quit

# install 1password
sudo rpm --import https://downloads.1password.com/linux/keys/1password.asc
sudo sh -c 'echo -e "[1password]\nname=1Password\nbaseurl=https://downloads.1password.com/linux/rpm\nenabled=1\ngpgcheck=1\nrepo_gpgcheck=1\ngpgkey=https://downloads.1password.com/linux/keys/1password.asc" > /etc/yum.repos.d/1password.repo'
sudo dnf install 1password

# install nodejs
curl -sL https://rpm.nodesource.com/setup_15.x | sudo bash -
sudo yum install -y nodejs

# set up postgresql (see https://developer.fedoraproject.org/tech/database/postgresql/about.html)
# initialize PG cluster
sudo postgresql-setup initdb
# start cluster
sudo systemctl start postgresql

source ~/.bashrc
