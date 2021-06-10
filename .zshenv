export EDITOR="emacsclient"        # $EDITOR opens in terminal
export VISUAL="emacsclient" # $VISUAL opens in GUI mode
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"

export LESSHISTFILE=- # no less history-file
export MAILDIR="$HOME/.mail"

# REVIEW: check if this directory exists?
PATH="$HOME/.local/bin:$PATH"

[[ -e $HOME/.profile ]] && emulate sh -c ". $HOME/.profile"

export ANDROID_SDK_ROOT="$XDG_DATA_HOME/Android/Sdk"
export ANDROID_HOME="$ANDROID_SDK_ROOT"
if [ -d $ANDROID_SDK_ROOT ]; then
   export PATH=$PATH:$ANDROID_SDK_ROOT/emulator
   export PATH=$PATH:$ANDROID_SDK_ROOT/tools
   export PATH=$PATH:$ANDROID_SDK_ROOT/tools/bin
   export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools
fi

export npm_config_prefix="$HOME/.local"
export npm_config_cache="$XDG_CACHE_HOME/npm"

export DENO_INSTALL=$XDG_DATA_HOME/deno

export CARGO_HOME=$XDG_DATA_HOME/cargo
export RUSTUP_HOME=$XDG_DATA_HOME/rustup

if [ -d $CARGO_HOME ]; then
  . "$CARGO_HOME/env"
fi

export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$HOME/.local/bin"

export NVIM_LOG_FILE=/tmp/nvim.log

