#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s checkwinsize

PS1='\[\e[0;36m\]\u\[\e[m\]@\[\e[0;32m\]\h\[\e[m\]:\W
$ '

export PATH=$PATH:~/bin
export EDITOR=vim
export VISUAL=vim

export HISTSIZE=1024
export HISTFILESIZE=100000
export HISTCONTROL=ignoredups:ignorespace:erasedups

alias la='ls -Fa --color=auto'
alias ll='ls -Fl --color=auto'
alias lla='ls -Fla --color=auto'

