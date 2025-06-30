#
# ~/.bashrc - Minimal Configuration
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Run fastfetch for system info display
fastfetch

# Start tmux automatically if not already in a tmux session
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux
fi

# Basic aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias nv='nvim'


# Basic history settings
export HISTSIZE=1000
export HISTFILESIZE=2000
export HISTCONTROL=ignoreboth
shopt -s histappend

# Enable bash completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
  . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi

# Enable fzf if available
if command -v fzf &> /dev/null; then
  eval "$(fzf --bash)"
fi

# Set PATH
export PATH="$PATH:/home/Duncan/.millennium/ext/bin"

# Set up starship prompt if available
if command -v starship &> /dev/null; then
  eval "$(starship init bash)"
else
  # Fallback PS1 if starship isn't available
  PS1='[\u@\h \W]\$ '
fi

alias cp='/usr/local/bin/cpg -g'
alias mv='/usr/local/bin/mvg -g'


export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - bash)"
