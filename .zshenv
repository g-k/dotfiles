PROMPT='%~> '


# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:$HOME/Library/Haskell/bin:${PATH}:/opt/local/bin"
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

