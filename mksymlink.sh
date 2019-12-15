#!/bin/sh

find ~/dotfiles/xwindow -maxdepth 1 -type f | while read file; do ln -Ffs $file ~/$(basename $file); done
find ~/dotfiles/zsh -maxdepth 1 -type f | while read file; do ln -Ffs $file ~/$(basename $file); done
find ~/dotfiles/vim -maxdepth 1 -type f | while read file; do ln -Ffs $file ~/$(basename $file); done
find ~/dotfiles/tmux -maxdepth 1 -type f | while read file; do ln -Ffs $file ~/$(basename $file); done

mkdir -p ~/bin
find ~/dotfiles/bin -maxdepth 1 -type f | while read file; do ln -Ffs $file ~/bin/$(basename $file); done

unlink ~/.vim
ln -s ~/dotfiles/vimfiles ~/.vim

