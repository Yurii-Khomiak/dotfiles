# Setting prompt
PS1_HOSTNAME_COLOR="yellow"
PS1_AT_COLOR="blue"
PS1_USERNAME_COLOR="green"
NEWLINE=$'\n'
PS1="[%F{${PS1_HOSTNAME_COLOR}}%M%f%F{${PS1_AT_COLOR}}@%f%F{${PS1_USERNAME_COLOR}}%n%f %~]${NEWLINE}$ "

# Enable autocompletion
autoload -Uz compinit
compinit

# History settings
HISTFILE=~/.config/zsh/.histfile
HISTSIZE=10000
SAVEHIST=10000

alias gdot="git --git-dir=$HOME/.config/dotgit/public/ --work-tree=$HOME"
alias ghci="ghci -ghci-script=$HOME/.config/ghci/startup"

search_and_open_in_editor() {
	local search_location=$1
	du -a "$search_location" \
		| cut -f2 \
		| fzf \
		| xargs -r $EDITOR
}

sc() { search_and_open_in_editor "$HOME/.config" }
ssc() { search_and_open_in_editor "$HOME/.local/bin/scripts" }

