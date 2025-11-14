#
# ~/.bashrc
#

PS1='\[\e[0;36m\]\u\[\e[m\]@\[\e[0;32m\]\h\[\e[m\]:\w
$ '

export PATH="${HOME}/bin:${HOME}/.local/bin:${PATH}"
export EDITOR=vim
export VISUAL=vim

alias ls='ls --color=auto'
alias la='ls -Fa --color=always'
alias ll='ls -Fl --color=always'
alias lla='ls -Fla --color=always'
#alias emacs='emacs -nw'
alias tmux='tmux -u'

# gauche
export GAUCHE_READ_EDIT=''

# rbenv
if [ -d ${HOME}/.rbenv ]; then
	export PATH=${HOME}/.rbenv/bin:${PATH}
	eval "$(rbenv init -)"
	export PATH=$(ruby -r rubygems -e 'puts Gem.user_dir')/bin:${PATH}
fi

# nvm
[ -z "${NVM_DIR}" ] && export NVM_DIR="${HOME}/.nvm"
[ -d "/usr/share/nvm" ] && nvm_init_dir="/usr/share/nvm" || nvm_init_dir="${NVM_DIR}"
if [ -d "${nvm_init_dir}" ]; then
	alias nvm='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && nvm'
	alias node='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && node'
	alias npm='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && npm'
	alias npx='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && npx'
fi

# cargo
[ -e "${HOME}/.cargo/env" ] && source "${HOME}/.cargo/env"

# roswell
[ -d "${HOME}/.roswell" ] && export PATH="${HOME}/.roswell/bin:${PATH}"

# pyenv
if [ -d "${HOME}/.pyenv" ]; then
	export PYENV_ROOT="${HOME}/.pyenv"
	if [ -d "${PYENV_ROOT}/bin" ]; then
		export PATH="${PYENV_ROOT}/bin:${PATH}"
		eval "$(pyenv init - bash)"
		eval "$(pyenv virtualenv-init -)"
		eval "$(register-python-argcomplete pipx)"
	fi
fi

# ghcup
if [ -d "${HOME}/.ghcup" ]; then
	export PATH="${HOME}/.ghcup/bin:${PATH}"
	if [ -d "${HOME}/.cabal" ]; then
		export PATH="${HOME}/.cabal/bin:${PATH}"
	fi
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s checkwinsize

bind -s 'set completion-ignore-case on'

export PROMPT_COMMAND='history -a'

export HISTSIZE=1024
export HISTFILESIZE=100000
export HISTCONTROL=ignoredups:ignorespace:erasedups

export LSCOLORS=ExFxCxdxBxegedabagacad

# fzf
export FZF_DEFAULT_OPTS='--ansi --border --reverse --height=80%'
if [ -e /usr/share/doc/fzf/examples/key-bindings.bash ]; then
	source /usr/share/doc/fzf/examples/key-bindings.bash
fi
if [ -e /usr/share/fzf/key-bindings.bash ] && [ -e /usr/share/fzf/completion.bash ]; then
	source /usr/share/fzf/key-bindings.bash
	source /usr/share/fzf/completion.bash
fi

# nvm
if [ -d "${nvm_init_dir}" ]; then
	alias nvm='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && nvm'
	alias node='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && node'
	alias npm='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && npm'
	alias npx='unalias nvm node npm npx && source "${nvm_init_dir}/nvm.sh" && source "${nvm_init_dir}/bash_completion" && npx'
fi

# fnm
FNM_PATH="${HOME}/.local/share/fnm"
if [ -d "$FNM_PATH" ]; then
  export PATH="$FNM_PATH:$PATH"
  eval "$(fnm env)"
fi

# pyenv
if [ -d "${HOME}/.pyenv" ] && [ -d "${PYENV_ROOT}/bin" ]; then
	eval "$(register-python-argcomplete pipx)"
fi

# Starship
if which starship > /dev/null 2>&1; then
	eval "$(starship init bash)"
fi

