export EDITOR='emacsclient -ca emacs'
export VISUAL="$EDITOR"
export BROWSER="firefox"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

export LESSHISTFILE=- # no less history-file

export MAILDIR="$HOME/.mail"

[[ -e ~/.profile ]] && emulate sh -c '. ~/.profile'

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# setup Go development
export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$GOPATH/bin"
if [ -d "$GOBIN" ] ; then
    PATH="$GOBIN:$PATH"
fi

# setup Deno
# export DENO_INSTALL="$HOME/.local/share/deno"
# if [ -d "$DENO_INSTALL" ] ; then
#     PATH="$DENO_INSTALL:$PATH"
# fi

# setup Rust
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export CARGO_HOME="$XDG_DATA_HOME"/cargo
if [ -d "$CARGO_HOME/bin" ] ; then
    PATH="$CARGO_HOME/bin:$PATH"
fi

# setup Android development
export ANDROID_SDK_ROOT=$XDG_DATA_HOME/Android/sdk
if [ -d "$ANDROID_SDK_ROOT" ]; then
    PATH="$ANDROID_SDK_ROOT/tools/bin:$PATH"
    PATH="$ANDROID_SDK_ROOT/platform-tools:$PATH"
    PATH="$ANDROID_SDK_ROOT/emulator:$PATH"
    PATH="$ANDROID_SDK_ROOT/build-tools:$PATH"
fi

export NVIM_LOG_FILE=/tmp/nvim.log
