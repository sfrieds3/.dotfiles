function flushdns -d "Flush OS X DNS cache"
    sudo dscacheutil -flushcache
    sudo killall -HUP mDNSResponder
end
