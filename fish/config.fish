set BIN_DIR $HOME/.local/bin
set PATH $BIN_DIR $PATH

# Android development
export ANDROID_HOME=$HOME/.local/share/Android/sdk
set PATH $ANDROID_HOME/tools/bin $PATH
set PATH $ANDROID_HOME/platform-tools $PATH
set PATH $ANDROID_HOME/emulator $PATH

# Go development
set PATH /usr/local/go/bin $PATH
export GOPATH=$HOME/.local/share/go
export GOBIN=$GOPATH/bin
set PATH $GOBIN $PATH

# Rust development
set PATH $HOME/.cargo/bin $PATH

# Deno
export DENO_INSTALL=$HOME/.local/share/.deno
set PATH $DENO_INSTALL/bin $PATH

# Default applications
set EDITOR nvim
set VISUAL code
set BROWSER firefox

export NVIM_LOG_FILE=/tmp/nvim.log
export BAT_THEME=OneHalfDark

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I'

alias du='du -sh'
alias free='free -h'

alias vi='nvim'
alias vim='nvim'
alias vimdiff='nvim -d'

alias python='python3'
alias venv='python -m venv'
