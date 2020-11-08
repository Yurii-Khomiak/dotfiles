# Setting prompt
PS1_HOSTNAME_COLOR="yellow"
PS1_AT_COLOR="blue"
PS1_USERNAME_COLOR="green"
NEWLINE=$'\n'
PS1="[%F{${PS1_HOSTNAME_COLOR}}%M%f%F{${PS1_AT_COLOR}}@%f%F{${PS1_USERNAME_COLOR}}%n%f %~]${NEWLINE}$ "

# Enable autocompletion
autoload -Uz compinit
compinit

# Enable VI mode
bindkey -v

# History settings
HISTFILE="$XDG_CONFIG_HOME/zsh/.histfile"
HISTSIZE=10000
SAVEHIST=10000

alias ll="ls -Al"
alias xmr="xmonad --recompile"
alias xmrr="xmonad --recompile && xmonad --restart"

alias gdot="git --git-dir=$XDG_CONFIG_HOME/dotgit/public/ --work-tree=$HOME"
alias ghci="ghci -ghci-script=$XDG_CONFIG_HOME/ghci/startup"

search_and_open_in_editor() {
	local search_location=$1
	find "$search_location" -type f\
		| cut -f2 \
		| fzf \
		| xargs -r $EDITOR
}

sc() { search_and_open_in_editor "$XDG_CONFIG_HOME" }
ssc() { search_and_open_in_editor "$HOME/.local/bin/scripts" }

