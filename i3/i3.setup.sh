#/bin/bash

DATETIME="`date +%Y%m%d%H%M%S`"
CONFIGDIR=$HOME/.config/i3

# see if we already have i3 config dir.. if we do, back it up
if [ -d "$CONFIGDIR" ]; then
    echo "$CONFIGDIR already exists, renaming as $CONFIGDIR.$DATETIME..."
    mv $CONFIGDIR $CONFIGDIR.$DATETIME
fi

# link all the things
echo "linking i3 config folder: ln -s $HOME/.dotfiles/i3/ $HOME/.config/i3/"
ln -s $HOME/.dotfiles/i3/ $HOME/.config/i3

# get our required packages
sudo dnf install -y ImageMagick
sudo dnf install -y scrot
sudo dnf install -y xautolock
sudo dnf install -y playerctl
sudo dnf install -y arandr
sudo dnf install -y fontawesome-fonts
sudo dnf install -y i3blocks
sudo dnf install -y brightnessctl
sudo dnf install -y rofi
sudo dnf install -y feh
sudo dnf install -y i3lock

# configure light for backlight stuff
echo "installing light: \
git clone https://github.com/haikarainen/light.git ~/.light \
cd ~/.light \
./autogen.sh \
./configure && make \
sudo make install"

git clone https://github.com/haikarainen/light.git ~/.light \
&& cd ~/.light \
&& ./autogen.sh \
&& ./configure && make \
&& sudo make install

echo "Adding scwfri to video group"
sudo usermod -a -G video scwfri
