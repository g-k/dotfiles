# set PATH
CABAL="$HOME/.cabal/bin"

PATH="$PATH:$HOME/bin:$CABAL"

if [[ `uname` = "Darwin" ]]; then
    HASKELL="$HOME/Library/Haskell/bin"
    PATH="$HASKELL:$PATH"

    HOMEBREW="/usr/local/bin:/usr/local/sbin:/usr/local/lib:/usr/local/share/python:/usr/local/Cellar/ruby/1.9.3-p194/bin"
    export PATH="$HOMEBREW:$PATH"
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

# programs
export EDITOR="emacs -nw"                # default editor
export SVN_EDITOR=$EDITOR

# aliases
alias gst="git status"
alias rm="nocorrect rm"

if [[ `uname` = "Linux" ]]; then
    # Install emacs 24 from ppa:cassou/emacs
    alias emacs='emacs-snapshot -nw'
else
    # Install emacs 24 with: brew upgrade emacs --HEAD --use-git-head
    alias emacs='emacs -nw'
fi
alias emacsd="emacs --daemon"
alias emacsc="emacsclient -t"

unsetopt correct_all

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
