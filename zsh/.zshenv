typeset -U PATH path
export EDITOR="emacsclient -t -a ''"              # $EDITOR use Emacs in terminal
export VISUAL="emacsclient -c -a emacs"           # $VISUAL use Emacs in GUI
# mode
export PAGER="less"
export TERMINAL="alacritty"
export BROWSER="firefox"

export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"

export LESSHISTFILE=- # no less history-file
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store"
export MAILDIR="$HOME/.mail"

# REVIEW: check if this directory exists?
if [ -d $HOME/.local/bin ]; then
  PATH="$HOME/.local/bin:$PATH"
fi

[[ -e $HOME/.profile ]] && emulate sh -c ". $HOME/.profile"

export ANDROID_SDK_ROOT="$XDG_DATA_HOME/Android/Sdk"
export ANDROID_HOME="$ANDROID_SDK_ROOT"
if [ -d $ANDROID_SDK_ROOT ]; then
   export PATH=$PATH:$ANDROID_SDK_ROOT/emulator
   export PATH=$PATH:$ANDROID_SDK_ROOT/tools
   export PATH=$PATH:$ANDROID_SDK_ROOT/tools/bin
   export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools
fi

export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java
#export ZDOTDIR=$HOME/.config/zsh
#export HISTFILE="$XDG_DATA_HOME"/zsh/history

# export EDITOR="nvim"
# export READER="zathura"
# export VISUAL="nvim"
# export TERMINAL="alacritty"
# export BROWSER="brave"
# export VIDEO="mpv"
# export IMAGE="sxiv"
# export COLORTERM="truecolor"
# export OPENER="xdg-open"
# export PAGER="less"
# export WM="bspwm"

export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc

export npm_config_prefix="$HOME/.local"
export npm_config_cache="$XDG_CACHE_HOME/npm"

if command -v yarn &> /dev/null
then
    export PATH="$(yarn global bin):$PATH"
fi

export DENO_INSTALL=$XDG_DATA_HOME/deno

export CARGO_HOME=$XDG_DATA_HOME/cargo
export RUSTUP_HOME=$XDG_DATA_HOME/rustup

if [ -d $CARGO_HOME ]; then
  . "$CARGO_HOME/env"
fi

export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$HOME/.local/bin"

export NVIM_LOG_FILE=/tmp/nvim.log
