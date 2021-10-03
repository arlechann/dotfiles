# Setup fzf

if [[ ! "$PATH" == */home/arle/.fzf/bin* ]] && [ ! -f "$(which fzf)" ]; then
  export PATH="${PATH:+${PATH}:}/home/arle/.fzf/bin"
fi

# Auto-completion

if [[ $- == *i* ]]; then
	[ -e "$HOME/.fzf/shell/completion.zsh" ] && source "$HOME/.fzf/shell/completion.zsh" 2> /dev/null
	[ -e "/usr/share/fzf/completion.zsh" ] && source "/usr/share/fzf/completion.zsh"
fi

export FZF_COMPLETION_OPTS='-m'

# Key bindings

#source "/home/arle/.fzf/shell/key-bindings.zsh"

