# .bash_profile

# Set PS1 for login shells (including tmux)
PS1='\[\e[0;36m\]\u\[\e[m\]@\[\e[0;32m\]\h\[\e[m\]:\w
$ '

# Source .bashrc if it exists and we're running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
fi
