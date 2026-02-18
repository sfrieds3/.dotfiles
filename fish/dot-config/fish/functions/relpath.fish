function relpath -d "Python-based relative path"
    python -c "import os,sys;print(os.path.relpath(*(sys.argv[1:]), start=os.path.expanduser('~')))" $argv
end
