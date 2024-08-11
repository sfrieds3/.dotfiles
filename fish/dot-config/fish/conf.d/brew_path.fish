# https://github.com/yangchenyun/fish-config/blob/89f56ea30fb5d620449732abf9be0cf20c5f91be/conf.d/brew.fish
# set -l brew_paths /opt/homebrew/bin /usr/local/bin /usr/bin /bin /opt/homebrew/sbin /usr/local/sbin /usr/sbin /sbin
#
# for brew_path in $brew_paths
#     if contains $brew_path $PATH
#         continue
#     end
#
#     if test -f $brew_path/brew
#         set PATH $brew_path $PATH
#         break
#     end
# end
#
# if not type -q brew
#     echo "Please install 'brew' first!"
# end
