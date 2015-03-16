local mash = {"cmd", "alt", "ctrl"}
local mashshift = {"cmd", "alt", "ctrl", "shift"}

hs.hints.style = "vimperator"

local caffeine = hs.menubar.new()
function setCaffeineDisplay(state)
    local result
    if state then
        result = caffeine:setIcon("caffeine-on.pdf")
    else
        result = caffeine:setIcon("caffeine-off.pdf")
    end
end

function caffeineClicked()
    setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
    caffeine:setClickCallback(caffeineClicked)
    setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end

hs.hotkey.bind(mashshift, "H", function()
  hs.window.focusedWindow():moveOneScreenWest()
end)

hs.hotkey.bind(mashshift, "J", function()
  hs.window.focusedWindow():moveOneScreenSouth()
end)

hs.hotkey.bind(mashshift, "K", function()
  hs.window.focusedWindow():moveOneScreenNorth()
end)

hs.hotkey.bind(mashshift, "L", function()
  hs.window.focusedWindow():moveOneScreenEast()
end)

hs.hotkey.bind(mash, "K", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h / 2
  win:setFrame(f)
end)

hs.hotkey.bind(mash, "J", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y + max.h / 2
  f.w = max.w
  f.h = max.h / 2
  win:setFrame(f)
end)

hs.hotkey.bind(mash, "H", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w * 0.66667
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind(mash, "L", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w * 0.66667)
  f.y = max.y
  f.w = max.w * 0.33334
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind(mash, "T", hs.hints.windowHints)

function reload_config(files)
    caffeine:delete()

    hs.reload()
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reload_config):start()
hs.alert.show("Config loaded")
