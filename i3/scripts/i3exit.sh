#!/bin/sh
lock() {
    HOSTNAME=$(uname -n)
    #~/.config/i3/scripts/lock_and_blur.sh
    wallpaperdir="${HOME}/.config/i3/wallpaper"
    case $HOSTNAME in
        mixolydian) wallpaper=southwest_harbor_3200_1800.png
            ;;
        phrygian) wallpaper=southwest_harbor_3840_2160.png
            ;;
        *) wallpaper=southwest_harbor.png
            ;;
    esac
    lockbg=$wallpaperdir/$wallpaper
    i3lock -i "${lockbg}"
    #echo $WALLPAPERDIR/$WALLPAPER
    #echo $lockbg
}

case "$1" in
    lock)
        $HOME/.config/i3/scripts/lock_and_blur.sh
        ;;
    logout)
        i3-msg exit
        ;;
    suspend)
        lock && systemctl suspend
        ;;
    hibernate)
        lock && systemctl hibernate
        ;;
    reboot)
        systemctl reboot
        ;;
    shutdown)
        systemctl poweroff
        ;;
    *)
        echo "Usage: $0 {lock|logout|suspend|hibernate|reboot|shutdown}"
        exit 2
esac

exit 0
