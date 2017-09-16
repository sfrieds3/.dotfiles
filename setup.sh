#!/bin/bash

#update git color for untracked files to white
git config color.status.untracked white
git config color.status.changed "white normal dim"
git config color.status.nobranch "red bold ul"
