function h2d
    set hex (echo $argv[1] | tr 'a-z' 'A-Z')
    echo "obase=10; ibase=16; $hex" | bc
end
