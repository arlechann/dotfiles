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

alias la='ls -Fa --color=always'
alias ll='ls -Fl --color=always'
alias lla='ls -Fla --color=always'

export FZF_DEFAULT_OPTS='--ansi --border --reverse --height=80%'

# rbenv
if [ -d ${HOME}/.rbenv ]; then
	export PATH="$HOME/.rbenv/bin:$(ruby -r rubygems -e 'puts Gem.user_dir')/bin:$PATH"
	eval "$(rbenv init -)"
fi

