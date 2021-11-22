#!/bin/sh

SCRIPT_PATH=$(cd $(dirname $0); pwd)

find "$SCRIPT_PATH/bash" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/$(basename $file)"; done
find "$SCRIPT_PATH/zsh" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/$(basename $file)"; done
find "$SCRIPT_PATH/vim" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/$(basename $file)"; done
find "$SCRIPT_PATH/tmux" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/$(basename $file)"; done

find "$SCRIPT_PATH/xwindow" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/$(basename $file)"; done
find "$SCRIPT_PATH/readline" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/$(basename $file)"; done
mkdir -p "$HOME/.config/i3"
find "$SCRIPT_PATH/i3" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/.config/i3/$(basename $file)"; done
mkdir -p "$HOME/.config/i3blocks"
find "$SCRIPT_PATH/i3blocks" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/.config/i3blocks/$(basename $file)"; done
mkdir -p "$HOME/.config/alacritty"
find "$SCRIPT_PATH/alacritty" -maxdepth 1 -type f | while read file; do ln -Ffs "$file" "$HOME/.config/alacritty/$(basename $file)"; done

unlink "$HOME/.vim"
ln -s "$SCRIPT_PATH/vimfiles" "$HOME/.vim"

[ -d "$SCRIPT_PATH/shellutils" ] || git clone https://github.com/arlechann/shellutils.git && (cd $SCRIPT_PATH/shellutils; ./install.sh)
