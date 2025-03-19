local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.hide_tab_bar_if_only_one_tab = true
config.color_scheme = 'Arthur'
config.font_size = 10.0

return config
