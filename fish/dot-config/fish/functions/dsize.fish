function dsize --description "Show sorted sizes of directories in current folder"
    du -h -d 1 . | sort -hr
end
