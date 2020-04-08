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

alias gdot="git --git-dir=$HOME/.cfg/ --work-tree=$HOME"

