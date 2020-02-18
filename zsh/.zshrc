# 補完を有効
autoload -Uz compinit
compinit

# UTF-8を利用
case "${OSTYPE}" in
	cygwin*)
		export LANG=ja_JP.UTF-8
		chcp 65001 > /dev/null 2> /dev/null
		;;
	linux*)
		;;
esac

# Emacsキーバインドを使う
bindkey -e

# 色
autoload -Uz colors
colors

# precmd
precmd() {
	vcs_info
	if [ -n "$TMUX" ]; then
		tmux refresh-client -S
	fi
}

# プロンプト
PROMPT="%(?,%F{white},%F{red})%(!,#,$)%f "
## git
autoload -Uz vcs_info
if [ -z "$TMUX" ]; then
	zstyle ':vcs_info:git:*' check-for-changes true
	zstyle ':vcs_info:git:*' stagedstr "%F{yellow}"
	zstyle ':vcs_info:git:*' unstagedstr "%F{red}"
	zstyle ':vcs_info:*' formats "%F{cyan}%c%u[%b]%f"
	zstyle ':vcs_info:*' actionformats "%F{red}[%b %a]%f"
	## 本体
	setopt prompt_subst
	PROMPT="%F{$(if [ "$UID" = 0 ]; then echo 'red'; else echo 'cyan'; fi)}%n%f@%F{green}%m%f:%F{white}%~%f \${vcs_info_msg_0_}
%(?,%F{white},%F{red})%(!,#,$)%f "
fi

# 環境変数DISPLAYの設定
#export DISPLAY=:0.0

# 環境変数path,PATHの重複を避ける
typeset -U path PATH

# historyの設定
export HISTFILE=${HOME}/.zsh_history
export HISTSIZE=1024
export SAVEHIST=16384
setopt hist_ignore_all_dups
setopt hist_verify
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_no_store
setopt hist_expand
#setopt inc_append_history
setopt EXTENDED_HISTORY

# globでno matchの警告を出さない
setopt nonomatch

# syntax highlight
if [ -f "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]; then
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# rbenvの設定
if [ -d ${HOME}/.rbenv ]; then
	export PATH="$HOME/.rbenv/bin:$(ruby -r rubygems -e 'puts Gem.user_dir')/bin:$PATH"
	eval "$(rbenv init -)"
fi

# pyenvの設定
if [ -d ${HOME}/.pyenv ]; then
	export PYENV_ROOT=$HOME/.pyenv
	export PATH=$PYENV_ROOT/bin:$PATH
	eval "$(pyenv init -)"
fi

# goの設定
if [ -d ${HOME}/go ]; then
	export PATH=${HOME}/go/bin:$PATH
fi

# nvmの設定
if [ -e /usr/share/nvm/init-nvm.sh ]; then
	source /usr/share/nvm/init-nvm.sh
fi

# roswellの設定
if [ -e ${HOME}/.roswell ]; then
	export PATH=${HOME}/.roswell/bin:$PATH
fi

# ~/bin
if [ -d ${HOME}/bin ]; then
	export PATH=$HOME/bin:$PATH
fi

# lsや補完候補をカラー表示す
export LSCOLORS=ExFxCxdxBxegedabagacad
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*' list-colors 'di=;34;1' 'ln=;35;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'

# 補完で大文字小文字区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# fzfの設定
if [ -f "$HOME/.fzf.zsh" ]; then
	source ~/.fzf.zsh
fi

if [ -f "$(which fzf)" ]; then
	export FZF_DEFAULT_OPTS='--ansi --border --reverse --height=50%'

	fzf-history() {
		BUFFER=$(history -r 1 | fzf --query "$LBUFFER" | cut -d' ' -f4-)
		CURSOR=${#BUFFER}
	}
	zle -N fzf-history
	bindkey '^r' fzf-history
fi

# tmuxを自動起動する
if [ -z "$TMUX" ] && [ $SHLVL = 1 ]; then
	tmux
fi

# Vimを使用する
export EDITOR="vim"
export VISUAL="vim"

# エイリアス設定
alias ls="ls --color"
alias ll="ls -l -F --color"
alias la="ls -a -F --color"
alias lla="ls -a -l -F --color"
alias emacs="emacs -nw"
alias crontab="crontab -i"

case "${OSTYPE}" in
	cygwin*)
		alias runx='run xwin -multiwindow -noclipboard'
		alias gvim="/cygdrive/D/Program\ Files/vim/vim74-kaoriya-win64/gvim.exe"
		;;
	linux*)
		alias vscode="code-oss"
		alias open="xdg-open"
		;;
esac

