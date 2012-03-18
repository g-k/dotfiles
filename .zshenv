# set PATH
CABAL="$HOME/.cabal/bin"
RUBYGEMS="/var/lib/gems/1.8/bin"

PATH="$PATH:$HOME/bin:$RUBYGEMS:$CABAL"

if [[ `uname` = "Darwin" ]]; then
    HASKELL="$HOME/Library/Haskell/bin"
    PATH="$HASKELL:$PATH"

    HOMEBREW="/usr/local/bin:/usr/local/sbin:/usr/local/lib:/usr/local/share/python"
    export PATH="$HOMEBREW:$PATH"

    PYTHONPATH="$PYTHONPATH:/Library/Python/2.6/site-packages"
fi
export PATH

export PYTHONPATH
export PYTHONSTARTUP="$HOME/.pythonrc"

# Source node version manager
if [[ -e ~/.nvm/nvm.sh ]]; then
    source ~/.nvm/nvm.sh
else
    git clone git://github.com/creationix/nvm.git ~/.nvm
    source ~/.nvm/nvm.sh
fi

source ~/.nodey.zsh # nodey-tools zsh completion

# Source python virtualenvwrapper
export WORKON_HOME=~/virtualenvs
if [[ -d $WORKON_HOME ]]; then
    mkdir -p $WORKON_HOME
fi
if [[ -e /usr/local/bin/virtualenvwrapper.sh ]]; then
    source /usr/local/bin/virtualenvwrapper.sh
else
    sudo pip install -U virtualenvwrapper
    source /usr/local/bin/virtualenvwrapper.sh
fi

# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=1000

# zsh settings
bindkey -e                      # Use emacs keybindings (-v for vi)
setopt hist_ignore_dups         # ignore repeated commands

# programs
export EDITOR="emacs -nw"                # default editor

# aliases
alias gst="git status"

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

