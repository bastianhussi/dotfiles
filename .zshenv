export EDITOR="nvim"
export VISUAL="emacs"
export BROWSER="firefox"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# setup Go development
export GOPATH="$HOME/.local/share/go"
export GOBIN="$GOPATH/bin"
if [ -d "$GOBIN" ] ; then
    PATH="$GOBIN:$PATH"
fi

# setup Deno
export DENO_INSTALL="$HOME/.local/share/deno"
if [ -d "$DENO_INSTALL" ] ; then
    PATH="$DENO_INSTALL:$PATH"
fi

# setup Rust
source "$HOME/.cargo/env"

# setup Android development
export ANDROID_SDK_ROOT=$HOME/.local/share/Android/sdk
if [ -d "$ANDROID_SDK_ROOT" ]; then
    PATH="$ANDROID_SDK_ROOT/tools/bin:$PATH"
    PATH="$ANDROID_SDK_ROOT/platform-tools:$PATH"
    PATH="$ANDROID_SDK_ROOT/emulator:$PATH"
    PATH="$ANDROID_SDK_ROOT/build-tools:$PATH"
fi

export NVIM_LOG_FILE=/tmp/nvim.log
