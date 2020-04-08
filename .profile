# Default programs
export PATH="$PATH:$(du $HOME/.local/bin | cut -f2 | sort | tr "\n" ":" | sed 's/:$//')"
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

# Start X if on first TTY and it's not already running
[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x Xorg >/dev/null && exec startx

