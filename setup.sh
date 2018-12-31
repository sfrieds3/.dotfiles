#!/bin/bash

# update git color for untracked files to white
git config --global color.status.untracked white
git config --global color.status.changed "white normal dim"
git config --global color.status.nobranch "red bold ul"

# update git color for diff
git config --global color.diff.old "yellow reverse dim"

# directory colors for ls
LS_COLORS='ow=01;36;40'
export LS_COLORS

# installations for neovim
go get -u github.com/jstemmer/gotags
go get -u github.com/nsf/gocode
go get github.com/rogpeppe/godef
go get -u github.com/derekparker/delve/cmd/dlv
gometalinter --install
export PATH=$PATH:$(go env GOPATH)/bin
cargo install racer
pip install jedi
pip3 install neovim
pip install neovim
sudo apt-get install exuberant-ctags
go get -u github.com/sourcegraph/go-langserver
pip install websocket-client sexpdata
