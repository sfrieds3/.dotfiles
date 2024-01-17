defaults write com.apple.finder AppleShowAllFiles TRUE
defaults write org.alacritty AppleFontSmoothing -int 0

# keyboard shortcuts

* in keyboard -> keyboard shortcuts -> app shortcuts:
    * set Quit alacritty and Quit Google Chrome to something other than Command-Q

# Alacritty config

* set up terminfo: sudo tic -xe alacritty,alacritty-direct extra/alacritty.info

# Docker compose with colima

```sh
brew install colima docker docker-compose docker-Buildx
```

```sh
# add docker compose to cli-plugins
mkdir -p ~/.docker/cli-plugins
ln -sfn $(brew --prefix)/opt/docker-compose/bin/docker-compose ~/.docker/cli-plugins/docker-compose

# add buildX to cli-plugins
ln -sfn $(brew --prefix)/opt/docker-buildx/bin/docker-buildx ~/.docker/cli-plugins/docker-buildx
```

# NOTE: for building python

``` sh
export CONFIGURE_OPTS="--with-openssl=$(brew --prefix openssl)"
asdf install python <version>
```
