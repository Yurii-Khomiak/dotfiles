export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWSTASHSTATE=true

# Setting prompt
setopt prompt_subst
source "$XDG_CONFIG_HOME"/zsh/git-prompt.sh
PS1_HOSTNAME="%F{yellow}%n%f"
PS1_AT="%F{blue}@%f"
PS1_USERNAME="%F{green}%M%f"
CURRENT_DIR="%~"
BRANCH=$'%F{yellow}($(__git_ps1 "%s"))%f'
NEWLINE=$'\n'
PS1="[${PS1_HOSTNAME}${PS1_AT}${PS1_USERNAME} ${CURRENT_DIR} ${BRANCH}]${NEWLINE}$ "

eval "$(oh-my-posh --config $XDG_CONFIG_HOME/ohmyposh/paradox.json init zsh)"

# Enable autocompletion
autoload -Uz compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

# Enable VI mode
bindkey -v

# History settings
HISTFILE="$XDG_CONFIG_HOME/zsh/.histfile"
HISTSIZE=10000
SAVEHIST=10000

alias_config="$XDG_CONFIG_HOME/zsh/.aliases"
[ -f "$alias_config" ] && source "$alias_config"

personal_config="$XDG_CONFIG_HOME/zsh/.personal"
[ -f "$personal_config" ] && source "$personal_config"

sp() {
	local project_dir="$HOME/myprojects"
	local search_result=$(find "$project_dir/" -maxdepth 3 -type d \
		| sed 's/.*myprojects\///' \
		| fzf)
	if [ ! -z "$search_result" ]; then
		local session_name="$(basename "$search_result" | tr . _)"

		tmux new-session -d -s "$session_name"

		tmux rename-window -t 1 nvim
		tmux send-keys -t "${session_name}:nvim" "cd \"$project_dir/$search_result\"" Enter "\"$EDITOR\" ." Enter

		tmux new-window -d -t 2 -n term
		tmux send-keys -t "${session_name}:term" "cd \"$project_dir/$search_result\"" Enter

		tmux attach-session -d -t "$session_name"
	fi
}

search_and_open_in_editor() {
	local search_location=$1
	local search_result=$(find "$search_location" -type f \
		| cut -f2 \
		| fzf)
	if [ ! -z "$search_result" ]; then
		cd "$(dirname "$search_result")"
		"$EDITOR" "$search_result"
	fi
}

sc() { search_and_open_in_editor "$XDG_CONFIG_HOME" }
sk() { search_and_open_in_editor "$PWD" }
ssc() { search_and_open_in_editor "$HOME/.local/bin/scripts" }

ZSH_HIGHLIGHT_PLUGIN="$XDG_DATA_HOME/zsh/highlight/zsh-syntax-highlighting.zsh"
source "$ZSH_HIGHLIGHT_PLUGIN"

