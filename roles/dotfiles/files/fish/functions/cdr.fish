function cdr -d "cd to root dir of repo"
  cd (git rev-parse --show-toplevel)
end
