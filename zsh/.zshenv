typeset -U path
path=(~/.local/bin ~/.pyenv/bin $path[@])

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
