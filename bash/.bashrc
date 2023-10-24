#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s checkwinsize

PS1='\[\e[0;36m\]\u\[\e[m\]@\[\e[0;32m\]\h\[\e[m\]:\w
$ '

export PATH=$PATH:~/bin
export PROMPT_COMMAND='history -a'
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
alias emacs='emacs -nw'

# fzf
export FZF_DEFAULT_OPTS='--ansi --border --reverse --height=80%'
if [ -e /usr/share/doc/fzf/examples/key-bindings.bash ]; then
	source /usr/share/doc/fzf/examples/key-bindings.bash
fi
if [ -e /usr/share/fzf/key-bindings.bash ] && [ -e /usr/share/fzf/completion.bash ]; then
	source /usr/share/fzf/key-bindings.bash
	source /usr/share/fzf/completion.bash
fi

# rbenv
if [ -d ${HOME}/.rbenv ]; then
	export PATH="$PATH:$HOME/.rbenv/bin"
	eval "$(rbenv init -)"
	export PATH="$PATH:$(ruby -r rubygems -e 'puts Gem.user_dir')/bin"
fi

# nvm
[ -z "$NVM_DIR" ] && export NVM_DIR="$HOME/.nvm"
[ -d "/usr/share/nvm" ] && nvm_init_dir="/usr/share/nvm" || nvm_init_dir="${NVM_DIR}"
if [ -d "${nvm_init_dir}" ]; then
	alias nvm='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && nvm'
	alias node='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && node'
	alias npm='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && npm'
	alias npx='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && npx'
fi

# cargo
[ -e ${HOME}/.cargo/env ] && source ${HOME}/.cargo/env

# roswell
if [ -d ${HOME}/.roswell ]; then
	export PATH=$PATH:$HOME/.roswell/bin
fi

# Starship
if which starship > /dev/null 2>&1; then
	eval "$(starship init bash)"
fi
