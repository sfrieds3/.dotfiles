function cleardns --description "Clear dns"
    sudo dscacheutil -flushcache && sudo killall -HUP mDNSResponder
end
