function notify --description "Generate MacOS Notification"
    if ! command -vq osascript
        echo "Only available on OS X"
        return
    end

    osascript -e 'display notification "'(string join ' ' $argv)'" with title "Terminal" sound name "Hero"'

end
