#! /usr/bin/python3

"""
Set up basic development environment on new machine
"""

from os.path import expanduser
import os


USER_HOME = expanduser("~")


def run():
    """
    set everything up
    """
    symlink_files()
    symlink_vim_folders()


def symlink_files():
    """
    add any files that need to be symlinked here
    """

    bash_files = [
        'vimrc'
    ]

    for file in bash_files:
        if not os.path.islink(USER_HOME + '/.' + file):
            print("ln -s {} {}".format(
                USER_HOME + '/.dotfiles/' + file,
                USER_HOME + '/.' + file
            ))
            os.symlink(
                USER_HOME + '/.dotfiles/' + file,
                USER_HOME + '/.' + file
            )


def symlink_vim_folders():
    """
    symlink necessary folders for vim
    """

    folder_list = [
        'autoload',
        'bundle',
        'colors'
    ]

    for file in folder_list:
        if not os.path.islink(USER_HOME + '/.vim/' + file):
            print("ln -s {} {}".format(
                USER_HOME + '/.dotfiles/_vim/' + file,
                USER_HOME + '/.vim/' + file
            ))
            os.symlink(
                USER_HOME + '/.dotfiles/_vim/' + file,
                USER_HOME + '/.vim/' + file,
                target_is_directory=True
            )


if __name__ == "__main__":
    run()
