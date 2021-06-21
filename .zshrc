autoload -Uz compinit
compinit
zstyle ':completion::complete:*' gain-privileges 1 # REVIEW: what does the `1` do?
zstyle ':completion:*' menu select=2 # selection style is enabled as soon as there are at least 2 entries to choose from

setopt COMPLETE_ALIASES

alias grep="grep --color=auto $1"
alias ls="ls --color=auto $1"
alias la="ls -Ahlv --color=auto $1"
alias du="du -sh $1"

# TODO: make if and then in one line
if command -v nvim &> /dev/null
then
    alias vi="nvim $1"
    alias vi="nvim $1"
    alias vim="nvim $1"
    alias vimdiff="nvim -d $1"
fi

# SEE: https://jdhao.github.io/2021/03/24/zsh_history_setup/
# the detailed meaning of the below three variable can be found in `man zshparam`.
# TODO: add fallback
export HISTFILE="$XDG_CACHE_HOME/zsh_history"
export HISTSIZE=1000   # the number of items for the internal history list
export SAVEHIST=10000   # maximum number of items for the history file

# The meaning of these options can be found in man page of `zshoptions`.
setopt HIST_IGNORE_ALL_DUPS  # do not put duplicated command into history list
setopt HIST_SAVE_NO_DUPS  # do not save duplicated command
setopt HIST_REDUCE_BLANKS  # remove unnecessary blanks
setopt INC_APPEND_HISTORY_TIME  # append command to history file immediately after execution
setopt EXTENDED_HISTORY  # record command start time

setopt AUTO_CD # change to a directory by typing its name

alias history="fc -l 1 $1"

# Define the theme
prompt_mytheme_setup() {
    PS1=""
    PS1+="%F{yellow}%n%f"
    PS1+="@"
#    PS1+="@%F{magenta}%m%f" # @hostname
    PS1+="%F{blue}%B%~%b%f" # working directory
    PS1+="%F{red}$(git_branch_name)%f" # git branch
    PS1+="%F{magenta}λ%f " # lambda sign
}

# SEE: https://medium.com/pareture/simplest-zsh-prompt-configs-for-git-branch-name-3d01602a6f33

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
# This line obtains information from the vcs.
zstyle ':vcs_info:git*' formats " (%b) "
precmd() {
    vcs_info
}

# Enable substitution in the prompt.
setopt prompt_subst

# Config for the prompt. PS1 synonym.
prompt='%2/ ${vcs_info_msg_0_}> '

ZSH_AUTOSUGGESTION="/usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
ZSH_SYNTAX_HIGHLIGHTING="/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

if [ -e $ZSH_AUTOSUGGESTION ]; then
  source $ZSH_AUTOSUGGESTION
fi

if [ -e $ZSH_SYNTAX_HIGHLIGHTING ]; then
  source $ZSH_SYNTAX_HIGHLIGHTING
fi

# Use Emacs bindings
# -v for Vim
bindkey -e

typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"

# setup key accordingly
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"         up-line-or-history
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"       down-line-or-history
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
  autoload -Uz add-zle-hook-widget
  function zle_application_mode_start { echoti smkx }
  function zle_application_mode_stop { echoti rmkx }
  add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
  add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

# History search
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search
