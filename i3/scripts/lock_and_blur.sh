#!/usr/bin/env bash

# from https://michaelabrahamsen.com/posts/custom-lockscreen-i3lock/
# set the icon and a temporary location for the screenshot to be stored
icon="$HOME/images/lock-icon-light.png"
tmpbg='/tmp/screen.png'

rm $tmpbg

# take a screenshot
scrot "$tmpbg"

# blur the screenshot by resizing and scaling back up
convert "$tmpbg" -filter Gaussian -thumbnail 10% -sample 1000% "$tmpbg"

# overlay the icon onto the screenshot
convert "$tmpbg" "$icon" -gravity center -composite "$tmpbg"

# lock the screen with the blurred screenshot
i3lock -n -i "$tmpbg"
