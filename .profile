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

# Cleaning home directory
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" # may break some DMs
export ZDOTDIR="$HOME/.config/zsh"
export LESSHISTSIZE="-"
export XMONAD_CONFIG_DIR="$HOME/.config/xmonad"
export XMONAD_CACHE_DIR="$HOME/.cache/xmonad"
export XMONAD_DATA_DIR="$HOME/.local/share/xmonad"
export NODE_REPL_HISTORY="$HOME/.cache/node/.node_repl_history"

export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"

# Start X if on first TTY and it's not already running
[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x Xorg >/dev/null && exec startx

sudo -n loadkeys "$HOME/.local/share/kbd/keymaps/personal.map"

