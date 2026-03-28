# dotfiles

個人用の dotfiles です。設定を `$HOME` 直下に並べず、ツールや用途ごとにディレクトリを分けて管理しています。

## 構成

- `bash`, `zsh`, `tmux`, `vim`, `lisp`, `readline`, `xwindow`
  `.bashrc` や `.tmux.conf` のような、ホームディレクトリ直下に置く設定です。
- `emacs`, `lem`
  エディタ設定です。実行時キャッシュやローカル状態は Git 管理の対象外です。
- `i3`, `i3blocks`, `alacritty`
  `~/.config/...` に配置する想定の XDG 系設定です。
- `windows`
  OS 固有の設定です。
- `codex`
  Codex 用のシェル補完です。
- `shellutils`
  別管理の補助スクリプト群です。

## インストール

基本の設定をリンクする場合:

```sh
./setup.sh
```

この場合は以下を行います。

- 主要なホームディレクトリ用 dotfiles をリンクする
- `zsh`、`readline`、`lem` も通常対象としてリンクする
- `vimfiles` を `~/.vim` にリンクする
- `emacs` を `~/.emacs.d` にリンクする
- `shellutils` の checkout があれば、その `install.sh` を実行する

X 関連や XDG 系の設定も含めてリンクする場合:

```sh
./setup.sh --x
```

`--x` を付けると、`xwindow`、`i3`、`i3blocks`、`alacritty` を追加でリンクします。

リンク先に通常のファイルやディレクトリがすでに存在する場合、`setup.sh` はそれを削除せず、警告を出してスキップします。

## Windows

Windows では `setup.bat` を使います。

```bat
setup.bat
```

このスクリプトは `--x` を扱いません。通常インストール相当の設定を `%%USERPROFILE%%` 配下へリンクします。
