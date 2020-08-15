#/usr/bin/sh

mkdir -p ~/.i3/scripts

ln -s ~/.dotfiles/lock_and_blur.sh ~/.i3/scripts/lock

sudo apt install imagemagick
sudo apt install scrot
sudo apt install xautolock
