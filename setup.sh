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
