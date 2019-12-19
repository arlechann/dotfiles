#!/bin/sh

DOTFILES=$(cd $(dirname $0); pwd)

find "$DOTFILES/zsh" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/$(basename $file)"; done
find "$DOTFILES/vim" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/$(basename $file)"; done
find "$DOTFILES/tmux" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/$(basename $file)"; done
find $DOTFILES/tmux/bin -maxdepth 1 -type f | while read file; do ln -Ffs $file $HOME/bin/$(basename $file); done

mkdir -p "$HOME/bin"
find "$DOTFILES/bin" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/bin/$(basename $file)"; done

find "$DOTFILES/xwindow" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/$(basename $file)"; done
mkdir -p "$HOME/.config/i3"
find "$DOTFILES/i3" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/.config/i3/$(basename $file)"; done
mkdir -p "$HOME/.config/i3blocks"
find "$DOTFILES/i3blocks" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/.config/i3blocks/$(basename $file)"; done

unlink "$HOME/.vim"
ln -s "$DOTFILES/vimfiles" "$HOME/.vim"

