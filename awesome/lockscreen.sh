#! /usr/bin/bash

wallpaper="$HOME/Pictures/Wallpaper/ClemsonHelmet.png"
lockscreen="$HOME/Pictures/lockscreen.png"

if ! [ -f $lockscreen ]; then
    convert -resize $(xdpyinfo | \grep dimensions | cut -d\  -f7 | cut -dx -f1) $wallpaper $lockscreen
fi

i3lock -n -i $lockscreen
