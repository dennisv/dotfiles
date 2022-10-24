import           XMonad
import qualified XMonad.StackSet               as W

import           Data.Monoid
import           XMonad.Actions.SpawnOn         ( spawnOn )
import           XMonad.Hooks.EwmhDesktops      ( addEwmhWorkspaceSort
                                                , ewmh
                                                , ewmhFullscreen
                                                )
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers     ( doFullFloat
                                                , isFullscreen
                                                )
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.Decoration
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation
import           XMonad.Util.Cursor
import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WorkspaceCompare   ( WorkspaceSort
                                                , filterOutWs
                                                )

myTerminal :: String
myTerminal = "alacritty "

myBrowser :: String
myBrowser = "firefox"

--brittany-disable-next-binding
main :: IO ()
main = xmonad $ docks . addEwmhWorkspaceSort (pure myFilterEwmh) . ewmhFullscreen . ewmh $ def
  { terminal = myTerminal
  , modMask = mod4Mask
  , borderWidth = 2
  , normalBorderColor = "#261E20"
  , focusedBorderColor = "#FF1F66"
  , layoutHook = myLayoutHook
  , manageHook = myManageHook <+> manageSpawn <+> namedScratchpadManageHook myScratchpads
  , startupHook = myStartupHook
  }
  `additionalKeysP` myKeys

myFilterEwmh :: WorkspaceSort
myFilterEwmh = filterOutWs filterList

filterList :: [String]
filterList = [scratchpadWorkspaceTag]

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr

  spawnOnce "polybar top &"
  spawn "nitrogen --restore &"


myLayoutHook
  :: ModifiedLayout
       AvoidStruts
       ( Choose
           ( ModifiedLayout
               SmartBorder
               ( ModifiedLayout
                   BoringWindows
                   ( ModifiedLayout
                       WindowNavigation
                       ( ModifiedLayout
                           (Decoration TabbedDecoration DefaultShrinker)
                           ( ModifiedLayout
                               (Sublayout Simplest)
                               (ModifiedLayout Spacing ResizableTall)
                           )
                       )
                   )
               )
           )
           ( Choose
               ( Mirror
                   ( ModifiedLayout
                       SmartBorder
                       ( ModifiedLayout
                           BoringWindows
                           ( ModifiedLayout
                               WindowNavigation
                               ( ModifiedLayout
                                   (Decoration TabbedDecoration DefaultShrinker)
                                   ( ModifiedLayout
                                       (Sublayout Simplest)
                                       (ModifiedLayout Spacing ResizableTall)
                                   )
                               )
                           )
                       )
                   )
               )
               (ModifiedLayout WithBorder Full)
           )
       )
       Window
myLayoutHook = avoidStruts myLayouts
 where
  myLayouts = tiled ||| Mirror tiled ||| full

  tiled =
    smartBorders
      $ boringWindows
      $ windowNavigation
      $ subTabbed
      $ spacing 5
      $ ResizableTall nmaster delta ratio []
  full    = noBorders Full

  nmaster = 1
  delta   = 3 / 100
  ratio   = toRational (2 / (1 + sqrt 5 :: Double))

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "telegram"
       "telegram-desktop"
       (appName =? "telegram-desktop")
       (customFloating $ W.RationalRect (2 / 3) (1 / 20) (1 / 3) (9 / 10))
  , NS "freetube"
       "freetube"
       (appName =? "freetube")
       (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "btop"
       (myTerminal ++ "--class btop --command btop")
       (appName =? "btop")
       (customFloating $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4))
  ]

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll [isFullscreen --> doFullFloat]

launchRofi :: MonadIO m => m ()
launchRofi = spawn "rofi -show drun"

launchShutdown :: MonadIO m => m ()
launchShutdown =
  spawn "rofi -show power -modi power:$HOME/.config/rofi/power.sh"

myKeys :: [(String, X ())]
myKeys =
  [ ("M-j"  , focusDown)
  , ("M-k"  , focusUp)
  , ("M-S-j", swapDown)
  , ("M-S-k", swapUp)
  , ("M-C-h", sendMessage $ pullGroup L)
  , ("M-C-j", sendMessage $ pullGroup D)
  , ("M-C-k", sendMessage $ pullGroup U)
  , ("M-C-l", sendMessage $ pullGroup R)
  , ("M-C-m", withFocused (sendMessage . MergeAll))
  , ("M-C-u", withFocused (sendMessage . UnMerge))
  , ("M-C-,", onGroup W.focusDown')
  , ("M-C-.", onGroup W.focusUp')
  , ("M-b"  , spawnOn "2" myBrowser)
  , ("M-S-c", namedScratchpadAction myScratchpads "telegram")
  , ("M-S-v", namedScratchpadAction myScratchpads "freetube")
  , ("M-S-t", namedScratchpadAction myScratchpads "btop")
  , ("M-p"  , launchRofi)
  , ("M-s"  , launchShutdown)
  ]
