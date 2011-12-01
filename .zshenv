# set PATH
CABAL="$HOME/.cabal/bin"
RUBYGEMS="/var/lib/gems/1.8/bin"

PATH="$PATH:$HOME/bin:$RUBYGEMS:$CABAL"

if [[ `uname` = "Darwin" ]]; then
    HASKELL="$HOME/Library/Haskell/bin"
    PATH="$HASKELL:$PATH"

    HOMEBREW="/usr/local/bin:/usr/local/sbin"
    PATH="$HOMEBREW:$PATH"

    HOMEBREW_PYTHON="/usr/local/lib/python2.6/site-packages"
    PYTHONPATH="$HOMEBREW_PYTHON:$PYTHONPATH:/Library/Python/2.6/site-packages"
fi
export PATH

export PYTHONPATH
export PYTHONSTARTUP="$HOME/.pythonrc"

# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=1000

# zsh settings
bindkey -e                      # Use emacs keybindings (-v for vi)
setopt autocd                   # cd by typing directory name
setopt hist_ignore_dups         # ignore repeated commands

# programs
export EDITOR="emacs -nw"                # default editor

# aliases
if ls -F --color=auto >&/dev/null; then
  eval `dircolors -b`
  alias ls="ls --color=auto -F"
else
  alias ls="ls -F"
fi
alias grep='grep --color=auto'

if [[ `uname` = "Linux" ]]; then
    # Install emacs 24 from ppa:cassou/emacs
    alias emacs='emacs-snapshot -nw'
else
    # Install emacs 24 with: brew upgrade emacs --HEAD --use-git-head
    alias emacs='emacs -nw'
fi



# tab completion
autoload -U compinit
zstyle ':completion:*' menu select list-colors ${(s.:.)LS_COLORS}

# colorful completion listings
zmodload -i zsh/complist
#zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

unsetopt correct_all

fpath=(~/.zsh/functions $fpath)

if [[ `uname` = "Darwin" ]]; then
    ulimit 40000
fi
