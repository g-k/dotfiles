which prompt &> /dev/null && prompt off

if [[ -r /usr/local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh ]]; then
    source /usr/local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh
fi

if [[ -r ~/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh ]]; then
    source ~/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh
fi

# set PATH
CABAL="$HOME/.cabal/bin"
PATH="$HOME/bin:$CABAL:$PATH"

if [[ `uname` = "Darwin" ]]; then
    HOMEBREW="/usr/local/share/npm/bin:/usr/local/bin:/usr/local/sbin:/usr/local/lib:/usr/local/lib/python2.7/site-packages"
    CUDA_BIN="/Developer/NVIDIA/CUDA-5.0/bin"
    PATH="$CUDA_BIN:$HOMEBREW:$PATH"
fi
export PATH

export PYTHONSTARTUP="$HOME/.pythonrc"

# load powerline shell
if [[ -r ~/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh ]]; then
    source ~/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh
fi

# zsh completions
source ~/.npm.zsh   # npm

# history
HISTFILE=~/.zsh_history

# zsh settings
setopt hist_ignore_dups         # ignore repeated commands
unsetopt correct_all

# programs
export EDITOR="emacs -nw"                # default editor
export SVN_EDITOR=$EDITOR

# aliases
alias gst="git status"
alias rm="nocorrect rm"
alias vlc=/Applications/VLC.app/Contents/MacOS/VLC

alias emacsd="emacs --daemon"
alias emacsc="emacsclient -t"


function start-ssh-agent; {
    eval "$(ssh-agent)"
    ssh-add ~/.ssh/id_rsa
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

# http://stackoverflow.com/questions/1554278/temporarily-put-away-uncommited-changes-in-subversion-a-la-git-stash

# fix for emscripten: http://permalink.gmane.org/gmane.comp.compilers.emscripten/1219
LLVM_ADD_VERSION=3.2

function svnstash; {
    svn diff > "$1".patch;
}

function svnapply; {
    patch -p0 < "$1"
}

function svnpop; {
    patch -p0 < "$1" && rm "$1"
}

function cuda; {
    export DYLD_LIBRARY_PATH=/Developer/NVIDIA/CUDA-5.0/lib:$DYLD_LIBRARY_PATH
}

function burp; {
    java -jar -Xmx1024m ~/Downloads/burpsuite_free_v1.6.jar
}

function stash-and-update-master; {
    git stash && git checkout master && git pull && git checkout - && git rebase master && git stash pop
}

source $HOME/.cargo/env


echo 'nautilus ~/Dropbox/read'
echo 'run: emacs -nw ~/Dropbox/TODO.org'
export ANDROID_HOME=/usr/local/opt/android-sdk

