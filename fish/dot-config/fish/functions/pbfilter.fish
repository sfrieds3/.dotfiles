function pbfilter -d "Filter pasteboard through a command"
    if test (count $argv) -gt 0
        pbpaste | $argv | pbcopy
    else
        pbpaste | pbcopy
    end
end
