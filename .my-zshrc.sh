# set PATH
CABAL="$HOME/.cabal/bin"
PATH="$PATH:$HOME/bin:$CABAL"

if [[ `uname` = "Darwin" ]]; then
    HASKELL="$HOME/Library/Haskell/bin"
    HOMEBREW="/usr/local/share/python:/usr/local/bin:/usr/local/sbin:/usr/local/lib"
    PATH="$HOMEBREW:$HASKELL:$PATH"
fi
export PATH

export PYTHONSTARTUP="$HOME/.pythonrc"

# zsh completions
source ~/.nodey.zsh # nodey-tools
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

alias emacsd="emacs --daemon"
alias emacsc="emacsclient -t"


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

function svnstash; {
    svn diff > "$1".patch;
}

function svnapply; {
    patch -p0 < "$1"
}

function svnpop; {
    patch -p0 < "$1" && rm "$1"
}

