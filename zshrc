# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="lambda"

# Example aliases
alias dj="python manage.py"
alias dr="dj runserver"
alias tmux="tmux -u"
alias tml="tmux list-sessions"
alias tma="tmux attach-session"
alias tmk="tmux kill-session -t"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(history-substring-search git mercurial brew npm pip gem fabric cloudapp tmuxinator vagrant)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
DEFAULT_USER="dennis"
export LC_ALL="en_US.UTF-8"
export TERM=xterm-256color
export HOMEBREW_NO_EMOJI=1
export EDITOR=vim
export PODFILE_TYPE=development

export GOPATH=$HOME/src/go

export PATH=$PATH:$GOPATH/bin
export PATH=$HOME/bin:$PATH

export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=$HOME/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=1

if [ -d /opt/boxen ]; then
  source /opt/boxen/env.sh
    source $BOXEN_HOME/homebrew/bin/virtualenvwrapper.sh
    export WORKON_HOME=$BOXEN_DATA_DIR/virturalenvs
fi
