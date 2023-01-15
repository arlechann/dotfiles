#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s checkwinsize

PS1='\[\e[0;36m\]\u\[\e[m\]@\[\e[0;32m\]\h\[\e[m\]:\w
$ '

export PATH=$PATH:~/bin
export EDITOR=vim
export VISUAL=vim

export HISTSIZE=1024
export HISTFILESIZE=100000
export HISTCONTROL=ignoredups:ignorespace:erasedups

export LSCOLORS=ExFxCxdxBxegedabagacad

export FZF_DEFAULT_OPTS='--ansi --border --reverse --height=80%'
export GAUCHE_READ_EDIT=''

alias ls='ls --color=auto'
alias la='ls -Fa --color=always'
alias ll='ls -Fl --color=always'
alias lla='ls -Fla --color=always'

export FZF_DEFAULT_OPTS='--ansi --border --reverse --height=80%'

# rbenv
if [ -d ${HOME}/.rbenv ]; then
	export PATH="$PATH:$HOME/.rbenv/bin"
	eval "$(rbenv init -)"
	export PATH="$PATH:$(ruby -r rubygems -e 'puts Gem.user_dir')/bin"
fi

# nvm
if [ -e "${NVM_DIR:-$HOME/.nvm}/nvm.sh" ]; then
	alias nvm='unalias nvm node npm && . "${NVM_DIR:-$HOME/.nvm}"/nvm.sh && nvm'
	alias node='unalias nvm node npm && . "${NVM_DIR:-$HOME/.nvm}"/nvm.sh && node'
	alias npm='unalias nvm node npm && . "${NVM_DIR:-$HOME/.nvm}"/nvm.sh && npm'
fi
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Starship
if which starship > /dev/null 2>&1; then
	eval "$(starship init bash)"
fi
