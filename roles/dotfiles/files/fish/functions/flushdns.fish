function flushdns -d "Flush OS X DNS cache"
    sudo killall -HUP mDNSResponder
end
