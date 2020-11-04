# Add .local/bin and it's dirs to PATH
export PATH="$PATH:$(du $HOME/.local/bin | \
	cut -f2 | \
	sort | \
	tr "\n" ":" | \
	sed 's/:$//')"

# Default programs
export EDITOR="nvim"
export TERMINAL="termite"
export BROWSER="chromium"

# XDG variables
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

# Cleaning home directory
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" # may break some DMs
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export LESSHISTFILE="$XDG_CACHE_HOME/.lesshst"
export XMONAD_CONFIG_DIR="$XDG_CONFIG_HOME/xmonad"
export XMONAD_CACHE_DIR="$XDG_CACHE_HOME/xmonad"
export XMONAD_DATA_DIR="$XDG_DATA_HOME/xmonad"
export NODE_REPL_HISTORY="$XDG_CACHE_HOME/node/.node_repl_history"
export YARN_CACHE_FOLDER="$XDG_CACHE_HOME/yarn"
export npm_config_cache="$XDG_CACHE_HOME/npm"

# For other programs
export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"

# Start X if on first TTY and it's not already running
[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x Xorg >/dev/null && exec startx

# Add "user host = (root) NOPASSWD: /usr/bin/loadkeys" to /etc/sudoers for this
# command to work
sudo -n loadkeys "$XDG_DATA_HOME/kbd/keymaps/personal.map"

