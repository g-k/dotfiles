eval "$(starship init zsh)"

# set PATH
PATH="$HOME/bin:$PATH"
export PATH

if [[ `uname` = "Darwin" ]]; then
    alias firefox=/Applications/Firefox.app/Contents/MacOS/firefox-bin
    alias vlc=/Applications/VLC.app/Contents/MacOS/VLC
fi

export PYTHONSTARTUP="$HOME/.pythonrc"

# zsh completions
source ~/.npm.zsh   # npm

# history
HISTFILE=~/.zsh_history

# zsh settings
setopt hist_ignore_dups         # ignore repeated commands
unsetopt correct_all

# programs
export EDITOR="emacs -nw"                # default editor

# aliases
alias gst="git status"
alias rm="nocorrect rm"
alias emacsd="emacs --daemon"
alias emacsc="emacsclient -t"

alias urldecode='python -c "import sys, urllib as ul; \
    print ul.unquote_plus(sys.argv[1])"'

function start-ssh-agent; {
    eval "$(ssh-agent)"
    ssh-add ~/.ssh/id_ed25519
}

function kill-ssh-agent; {
    eval "$(ssh-agent -k)"
}

## Functions: from @webcoyote

# URL encode something and print it.
function url-encode; {
  setopt extendedglob
  echo "${${(j: :)@}//(#b)(?)/%$[[##16]##${match[1]}]}"
}

# Search google
function google; {
  python -m webbrowser -t "http://www.google.com/search?q=$(url-encode ${(j: :)@})"
}

# Make directory and change to it
mdc() { mkdir -p "$1" && cd "$1" }

function stash-and-update-master; {
    git stash && git checkout master && git pull && git checkout - && git rebase master && git stash pop
}

function dump-memcached; {
    MEMCHOST=localhost; printf "stats items\n" | nc $MEMCHOST 11211 | grep ":number" | awk -F":" '{print $2}' | xargs -I % printf "stats cachedump % 0\r\n" | nc $MEMCHOST 11211 | grep ITEM | awk '{print $2}' | sed -e 's/"/\\"/g'| xargs -I % printf "get %\r\n" | nc $MEMCHOST 11211
}

function restart-mouse; {
    sudo modprobe -r psmouse && sudo modprobe psmouse;
}
source $HOME/.cargo/env
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

eval "$(pyenv init -)"

export ANDROID_HOME=/usr/local/opt/android-sdk

# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]; then . "$HOME/google-cloud-sdk/path.zsh.inc"; fi

## zsh completions

# The next line enables shell command completion for gcloud.
if [ -f "$HOME/google-cloud-sdk/completion.zsh.inc" ]; then . "$HOME/google-cloud-sdk/completion.zsh.inc"; fi

source ~/.npm.zsh   # npm
# source <(kubectl completion zsh)
# source <(minikube completion zsh)
# ~/.zfunc/_poetry

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
  # use go1.16
  export PATH="/opt/homebrew/opt/go@1.16/bin:$PATH"
  autoload -Uz compinit
  compinit
fi

export AWS_VAULT_BACKEND=pass
export AWS_VAULT_PASS_PREFIX=aws-vault/
