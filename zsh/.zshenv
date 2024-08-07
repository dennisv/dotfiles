typeset -U path
path=(~/.local/bin ~/.ghcup/bin $path[@])

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export GHCUP_USE_XDG_DIRS=1
