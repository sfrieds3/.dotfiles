function __make_python --argument _j --description "Make nvim from soure"
  ./configure --enable-optimizations --prefix $HOME/bin/python/ && make && make altinstall
end
