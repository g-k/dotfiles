# set PATH
CABAL="$HOME/.cabal/bin"

PATH="$PATH:$HOME/bin:$CABAL"

if [[ `uname` = "Darwin" ]]; then
    HASKELL="$HOME/Library/Haskell/bin"
    PATH="$HASKELL:$PATH"

    HOMEBREW="/usr/local/bin:/usr/local/sbin:/usr/local/lib:/usr/local/share/python"
    export PATH="$HOMEBREW:$PATH"
fi
export PATH

export PYTHONSTARTUP="$HOME/.pythonrc"

# Source node version manager
if [[ -e ~/.nvm/nvm.sh ]]; then
    # git clone git://github.com/creationix/nvm.git ~/.nvm
    # source ~/.nvm/nvm.sh
fi

# zsh completions
source ~/.nodey.zsh # nodey-tools
source ~/.npm.zsh   # npm

# Source python virtualenvwrapper
if [[ -e /usr/local/bin/virtualenvwrapper.sh ]]; then
    # pip install -U virtualenvwrapper
    source /usr/local/bin/virtualenvwrapper.sh
fi

# history
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=1000

# zsh settings
setopt hist_ignore_dups         # ignore repeated commands

# programs
export EDITOR="emacs -nw"                # default editor

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
