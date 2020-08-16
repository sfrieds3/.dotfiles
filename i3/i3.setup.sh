#/bin/bash

DATETIME="`date +%Y%m%d%H%M%S`"
CONFIGDIR=$HOME/.CONFIG/i3

# see if we already have i3 config dir.. if we do, back it up
if [ -d "$CONFIGDIR" ]; then
    echo "$CONFIGDIR already exists, renaming as $CONFIGDIR.$DATETIME..."
    mv $CONFIGDIR $CONFIGDIR.$DATETIME
fi

# link all the things
echo "linking i3 config folder: ln -s $HOME/.dotfiles/i3/ $HOME/.config/i3/"
ln -s $HOME/.dotfiles/i3/ $HOME/.config/i3/

# get our required packages
sudo apt update
sudo apt install -y imagemagick
sudo apt install -y scrot
sudo apt install -y xautolock
sudo apt install -y light # TODO: this may not work?
sudo apt install -y playerctl
sudo apt install -y arandr
sudo apt install -y fonts-font-awesome

