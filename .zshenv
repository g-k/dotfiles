# set paths
HASKELL="$HOME/Library/Haskell/bin"
CABAL="$HOME/.cabal/bin"
PY27="/Library/Frameworks/Python.framework/Versions/2.7/bin"
HOMEBREW="/usr/local/sbin:/usr/local/bin"
MONGO="$HOME/mongodb-osx-x86_64-1.8.1/bin"
RUBYGEMS="/var/lib/gems/1.8/bin"

PATH="$HASKELL:$PY27:$MONGO:$HOMEBREW:$PATH:$HOME/bin:$RUBYGEMS:$CABAL"
set PATH

# virtualenvwrapper
WORKON_HOME=$HOME/envs
set WORKON_HOME
source virtualenvwrapper.sh

# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=1000

# Emacs keybindings (-v for vi)
bindkey -e

# settings
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

alias emacs='emacs -nw'


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
