PROMPT='%~> '

# set paths
HASKELL="$HOME/Library/Haskell/bin"
PY27="/Library/Frameworks/Python.framework/Versions/2.7/bin"
MACPORTS="/opt/local/bin"
MONGO="/Users/gregg/mongodb-osx-x86_64-1.8.1/bin"
PATH="$HASEKLL_PATH:$PY27:$MONGO:$MACPORTS:${PATH}"

set PATH

# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=1000

# Emacs keybindings (-v for vi)
bindkey -e

# settings
#setopt correct_all              # correct misspelled commands
setopt autocd                   # cd by typing directory name
setopt hist_ignore_dups         # ignore repeated commands

# programs
export EDITOR=emacs                # default editor

# aliases
if ls -F --color=auto >&/dev/null; then
  eval `dircolors -b`
  alias ls="ls --color=auto -F"
else
  alias ls="ls -F"
fi
alias grep='grep --color=auto'

# tab completion
autoload -U compinit promptinit
compinit
zstyle ':completion:*' menu select list-colors ${(s.:.)LS_COLORS}

# colorful completion listings
zmodload -i zsh/complist
#zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

cd ~/evo
