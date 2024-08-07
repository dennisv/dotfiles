ZPLUGINDIR=${ZPLUGINDIR:-${ZDOTDIR:-$HOME/.config/zsh}/plugins}
if [[ ! -d $ZPLUGINDIR/zsh_unplugged ]]; then
  git clone --quiet https://github.com/mattmc3/zsh_unplugged $ZPLUGINDIR/zsh_unplugged
fi
source $ZPLUGINDIR/zsh_unplugged/zsh_unplugged.zsh
repos=(
  subnixr/minimal
  larkery/zsh-histdb
  zdharma-continuum/fast-syntax-highlighting
  zsh-users/zsh-history-substring-search
)
plugin-load $repos

if [[ -f $HOME/.asdf/asdf.sh ]]; then
  source $HOME/.asdf/asdf.sh
  fpath=(${ASDF_DIR}/completions $fpath)
fi

autoload -Uz add-zsh-hook
source $ZPLUGINDIR/zsh-histdb/histdb-interactive.zsh
bindkey '^r' _histdb-isearch

zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
autoload -Uz compinit && compinit

autoload -Uz promptinit && promptinit

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

bindkey '^?' backward-delete-char                     # [Backspace] - delete backward
if [[ "${terminfo[kdch1]}" != "" ]]; then
  bindkey "${terminfo[kdch1]}" delete-char            # [Delete] - delete forward
else
  bindkey "^[[3~" delete-char
  bindkey "^[3;5~" delete-char
  bindkey "\e[3~" delete-char
fi

[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
HISTSIZE=50000
SAVEHIST=10000

MNML_PROMPT=(mnml_ssh mnml_pyenv mnml_status 'mnml_cwd 1 0' mnml_git mnml_keymap)
MNML_RPROMPT=()

export PATH="$HOME/bin:$PATH"

alias ls='ls --color=auto'

export FLYCTL_INSTALL="$HOME/.fly"
export PATH="$FLYCTL_INSTALL/bin:$PATH"

eval "$(direnv hook zsh)"

source /home/dennis/.config/broot/launcher/bash/br
