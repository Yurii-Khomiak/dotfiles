# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

export PATH="$PATH:$(du $HOME/.local/bin | \
	cut -f2 | \
	sort | \
	tr "\n" ":" | \
	sed 's/:$//')"

export PATH="$PATH:/usr/local/go/bin"

# Default programs
export TERMINAL="xfce4-terminal"
export EDITOR="nvim"
export BROWSER="brave-browser"

# XDG variables
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# Cleaning home directory
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export LESSHISTFILE="-"
export NODE_REPL_HISTORY="$XDG_CACHE_HOME/node/.node_repl_history"
export YARN_CACHE_FOLDER="$XDG_CACHE_HOME/yarn"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm"

# For other programs
export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi



# Added by Toolbox App
export PATH="$PATH:/home/djurii/.local/share/JetBrains/Toolbox/scripts"

