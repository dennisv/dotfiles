local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.hide_tab_bar_if_only_one_tab = true
config.color_scheme = 'Arthur'
config.window_background_opacity = 0.9
config.font_size = 10.0

return config
