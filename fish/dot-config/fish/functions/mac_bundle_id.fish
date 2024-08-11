function mac_bundle_id --description "Return bundle id info for application"
    if test (count $argv) -eq 0
        echo "Please provide application name"
        exit
    end
    codesign -dr - $argv[1]
end
