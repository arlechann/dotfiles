#!/bin/sh

set -eu

INSTALL_X_CONFIGS=0

SCRIPT_PATH=$(
  cd "$(dirname "$0")"
  pwd
)

usage() {
  cat <<'EOF'
Usage: ./setup.sh [--x]

Options:
  --x       Install X/XDG-related configs such as i3, i3blocks, alacritty,
            and xwindow.
  -h, --help  Show this help.
EOF
}

warn() {
  printf 'warning: %s\n' "$*" >&2
}

link_path() {
  src=$1
  dst=$2

  if [ -L "$dst" ]; then
    ln -sfn "$src" "$dst"
    return
  fi

  if [ -e "$dst" ]; then
    warn "skip existing path: $dst"
    return
  fi

  ln -s "$src" "$dst"
}

link_dir_to_home() {
  src_dir=$1

  find "$src_dir" -maxdepth 1 -type f | while read -r file; do
    link_path "$file" "$HOME/$(basename "$file")"
  done
}

link_dir_to_config() {
  src_dir=$1
  config_name=$2
  target_dir=$HOME/.config/$config_name

  mkdir -p "$target_dir"
  find "$src_dir" -maxdepth 1 -type f | while read -r file; do
    link_path "$file" "$target_dir/$(basename "$file")"
  done
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --x)
      INSTALL_X_CONFIGS=1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      warn "unknown option: $1"
      usage
      exit 1
      ;;
  esac
  shift
done

link_dir_to_home "$SCRIPT_PATH/bash"
#link_dir_to_home "$SCRIPT_PATH/zsh"
link_dir_to_home "$SCRIPT_PATH/vim"
link_dir_to_home "$SCRIPT_PATH/tmux"
link_dir_to_home "$SCRIPT_PATH/lisp"
#link_dir_to_home "$SCRIPT_PATH/readline"

if [ "$INSTALL_X_CONFIGS" = "1" ]; then
  link_dir_to_home "$SCRIPT_PATH/xwindow"
  link_dir_to_config "$SCRIPT_PATH/i3" "i3"
  link_dir_to_config "$SCRIPT_PATH/i3blocks" "i3blocks"
  link_dir_to_config "$SCRIPT_PATH/alacritty" "alacritty"
fi

link_path "$SCRIPT_PATH/vimfiles" "$HOME/.vim"
link_path "$SCRIPT_PATH/emacs" "$HOME/.emacs.d"
#link_path "$SCRIPT_PATH/lem" "$HOME/.lem"

if [ -d "$SCRIPT_PATH/shellutils" ]; then
  (
    cd "$SCRIPT_PATH/shellutils"
    ./install.sh
  )
else
  warn "shellutils checkout is missing: $SCRIPT_PATH/shellutils"
fi
