# set paths
HASKELL="$HOME/Library/Haskell/bin"
CABAL="$HOME/.cabal/bin"
HOMEBREW="/usr/local/sbin:/usr/local/bin"
RUBYGEMS="/var/lib/gems/1.8/bin"

PATH="$HASKELL:$HOMEBREW:$PATH:$HOME/bin:$RUBYGEMS:$CABAL"
set PATH

PYTHONPATH="/Library/Python/2.6/site-packages:$PYTHONPATH"
export PYTHONPATH

export PYTHONSTARTUP="$HOME/.pythonrc"


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
