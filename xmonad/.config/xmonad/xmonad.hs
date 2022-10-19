import           XMonad

import           Data.Monoid
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , ewmhFullscreen
                                                )
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers     ( doFullFloat
                                                , isFullscreen
                                                )
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.SpawnOnce

--brittany-disable-next-binding
main :: IO ()
main = xmonad $ docks $ ewmhFullscreen $ ewmh $ def
  { terminal = "alacritty"
  , modMask = mod4Mask
  , borderWidth = 2
  , normalBorderColor = "#261E20"
  , focusedBorderColor = "#FF1F66"
  , layoutHook = myLayoutHook
  , manageHook = myManageHook
  , startupHook = myStartupHook
  }
  `additionalKeysP` myKeys

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "polybar top &"
  spawnOnce "nitrogen --restore &"

myLayoutHook = avoidStruts myLayouts
 where
  myLayouts = tiled ||| Mirror tiled ||| full

  tiled     = spacing 5 $ smartBorders (ResizableTall nmaster delta ratio [])
  full      = noBorders Full

  nmaster   = 1
  delta     = 3 / 100
  ratio     = toRational (2 / (1 + sqrt 5 :: Double))

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll [isFullscreen --> doFullFloat]

cleanStart :: MonadIO m => m ()
cleanStart = spawn "xmonad --recompile && xmonad --restart"

launchRofi :: MonadIO m => m ()
launchRofi = spawn "rofi -show drun"

launchShutdown :: MonadIO m => m ()
launchShutdown =
  spawn "rofi -show power -modi power:$HOME/.config/rofi/power.sh"

myKeys :: [(String, X ())]
myKeys =
  [ ("M-p", launchRofi) -- dmenu app launcher
  , ("M-s", launchShutdown) -- dmenu app launcher
  , ("M-q", cleanStart) -- restart xmonad
  ]
