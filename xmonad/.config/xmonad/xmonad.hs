import           Data.Bifunctor                 ( bimap )
import           Data.Char                      ( isSpace )
import           Data.List                      ( dropWhileEnd
                                                , elemIndex
                                                , find
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Monoid                    ( Endo )
import           System.IO.Unsafe               ( unsafeDupablePerformIO )

import           XMonad
import qualified XMonad.StackSet               as W

import           XMonad.Actions.SpawnOn         ( spawnOn )
import           XMonad.Hooks.EwmhDesktops      ( addEwmhWorkspaceSort
                                                , ewmh
                                                , ewmhFullscreen
                                                )
import           XMonad.Hooks.ManageDocks       ( AvoidStruts
                                                , Direction2D(D, L, R, U)
                                                , avoidStruts
                                                , docks
                                                )
import           XMonad.Hooks.ManageHelpers     ( doCenterFloat
                                                , doFullFloat
                                                , isFullscreen
                                                )
import           XMonad.Layout.BoringWindows    ( BoringWindows
                                                , boringWindows
                                                , focusDown
                                                , focusUp
                                                , swapDown
                                                , swapUp
                                                )
import           XMonad.Layout.Decoration       ( Decoration
                                                , DefaultShrinker
                                                , Theme(..)
                                                , shrinkText
                                                )
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.NoBorders        ( SmartBorder
                                                , WithBorder
                                                , noBorders
                                                , smartBorders
                                                )
import           XMonad.Layout.ResizableTile    ( ResizableTall(ResizableTall) )
import           XMonad.Layout.Simplest         ( Simplest(Simplest) )
import           XMonad.Layout.Spacing          ( Spacing
                                                , smartSpacingWithEdge
                                                )
import           XMonad.Layout.SubLayouts       ( GroupMsg
                                                  ( MergeAll
                                                  , UnMerge
                                                  , UnMergeAll
                                                  )
                                                , Sublayout
                                                , onGroup
                                                , pullGroup
                                                , subLayout
                                                )
import           XMonad.Layout.Tabbed           ( TabbedDecoration
                                                , addTabs
                                                )
import           XMonad.Layout.WindowNavigation ( WindowNavigation
                                                , configurableNavigation
                                                , noNavigateBorders
                                                )
import           XMonad.Util.Cursor             ( setDefaultCursor )
import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Util.NamedScratchpad    ( NamedScratchpad(NS)
                                                , customFloating
                                                , namedScratchpadAction
                                                , namedScratchpadManageHook
                                                , scratchpadWorkspaceTag
                                                )
import           XMonad.Util.Run                ( runProcessWithInput )
import           XMonad.Util.SpawnOnce          ( manageSpawn
                                                , spawnOnce
                                                )
import           XMonad.Util.WorkspaceCompare   ( WorkspaceSort
                                                , filterOutWs
                                                )

myTerminal :: String
myTerminal = "alacritty "

myBrowser :: String
myBrowser = "firefox"

main :: IO ()
main =
  xmonad
    . docks
    . addEwmhWorkspaceSort (pure myFilterEwmh)
    . ewmhFullscreen
    . ewmh
    $ myConfig
 where
  myConfig =
    def
        { terminal           = myTerminal
        , modMask            = mod4Mask
        , borderWidth        = 2
        , normalBorderColor  = base00
        , focusedBorderColor = base08
        , layoutHook         = myLayoutHook
        , manageHook         = myManageHook
                               <+> manageSpawn
                               <+> namedScratchpadManageHook myScratchpads
        , startupHook        = myStartupHook
        }
      `additionalKeysP` myKeys

myFilterEwmh :: WorkspaceSort
myFilterEwmh = filterOutWs filterList

filterList :: [String]
filterList = [scratchpadWorkspaceTag]

myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr

  spawnOnce "polybar top &"
  spawn "wal"


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
      . boringWindows
      . configurableNavigation noNavigateBorders
      . addTabs shrinkText myTabTheme
      . subLayout [] Simplest
      . smartSpacingWithEdge 4
      $ ResizableTall nmaster delta ratio []
  full    = noBorders Full

  nmaster = 1
  delta   = 3 / 100
  ratio   = toRational (2 / (1 + sqrt 5 :: Double))

myTabTheme :: Theme
myTabTheme = def
  { activeColor         = base08
  , activeBorderColor   = base08
  , activeTextColor     = basefg
  , activeBorderWidth   = 0
  , inactiveColor       = base00
  , inactiveBorderColor = basebg
  , inactiveTextColor   = base07
  , inactiveBorderWidth = 2
  , fontName = "xft:Iosevka:regular:size=9:antialias=true:hinting=true"
  , decoHeight          = 25
  , decoWidth           = maxBound
  }

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "mu4e"
       "emacs --name=mu4e --title=@mail -f mu4e"
       (appName =? "mu4e")
       (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "telegram"
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

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [appName =? "1password" --> doCenterFloat, isFullscreen --> doFullFloat]

launchRofi :: MonadIO m => m ()
launchRofi = spawn "rofi -show drun"

launchShutdown :: MonadIO m => m ()
launchShutdown =
  spawn "rofi -show power -modi power:$HOME/.config/rofi/power.sh"

myKeys :: [(String, X ())]
myKeys =
  [ ("M-j"    , focusDown)
  , ("M-k"    , focusUp)
  , ("M-S-j"  , swapDown)
  , ("M-S-k"  , swapUp)
  , ("M-C-h"  , sendMessage $ pullGroup L)
  , ("M-C-j"  , sendMessage $ pullGroup D)
  , ("M-C-k"  , sendMessage $ pullGroup U)
  , ("M-C-l"  , sendMessage $ pullGroup R)
  , ("M-C-m"  , withFocused (sendMessage . MergeAll))
  , ("M-C-u"  , withFocused (sendMessage . UnMerge))
  , ("M-C-S-u", withFocused (sendMessage . UnMergeAll))
  , ("M-C-,"  , onGroup W.focusDown')
  , ("M-C-."  , onGroup W.focusUp')
  , ("M-b"    , spawnOn "2" myBrowser)
  , ("M-S-m", namedScratchpadAction myScratchpads "mu4e")
  , ("M-S-n", namedScratchpadAction myScratchpads "telegram")
  , ("M-S-v", namedScratchpadAction myScratchpads "freetube")
  , ("M-S-t", namedScratchpadAction myScratchpads "btop")
  , ("M-p"    , launchRofi)
  , ("M-s"    , launchShutdown)
  ]

{- |
   Module : Theme.Theme
   Copyright : (c) 2021 Joan Milev <joantmilev@gmail.com>
   License : MIT

   Maintainer : Joan Milev <joantmilev@gmail.com>
   Stability : Stable
   Portability : Unknown
-}

basebg, basefg, basecr, base00, base08, base01, base09, base02, base0A, base03, base0B, base04, base0C, base05, base0D, base06, base0E, base07, base0F
  :: String
basebg = xprop "*.background"
basefg = xprop "*.foreground"
basecr = xprop "*.cursorColor"
base00 = xprop "*.color0"
base08 = xprop "*.color8"
base01 = xprop "*.color1"
base09 = xprop "*.color9"
base02 = xprop "*.color2"
base0A = xprop "*.color10"
base03 = xprop "*.color3"
base0B = xprop "*.color11"
base04 = xprop "*.color4"
base0C = xprop "*.color12"
base05 = xprop "*.color5"
base0D = xprop "*.color13"
base06 = xprop "*.color6"
base0E = xprop "*.color14"
base07 = xprop "*.color7"
base0F = xprop "*.color15"

-- myFont, myFontGTK, myBigFont, myBoldFont, myItalicFont :: String
-- myFont = xprop "xmonad.font"
-- myFontGTK = xprop "xmonad.font.gtk"
-- myBigFont = xprop "xmonad.font.big"
-- myBoldFont = xprop "xmonad.font.bold"
-- myItalicFont = xprop "xmonad.font.italic"

{- |
   Module : Theme.Xresources
   Copyright : (c) 2021 Joan Milev <joantmilev@gmail.com>
   License : MIT

   Maintainer : Joan Milev <joantmilev@gmail.com>
   Stability : Stable
   Portability : Unknown
-}

xProperty :: String -> IO String
xProperty key =
  fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""

findValue :: String -> String -> Maybe String
findValue xresKey xres =
  snd <$> find ((== xresKey) . fst) (catMaybes $ splitAtColon <$> lines xres)

splitAtColon :: String -> Maybe (String, String)
splitAtColon str = splitAtTrimming str <$> elemIndex ':' str

splitAtTrimming :: String -> Int -> (String, String)
splitAtTrimming str idx = bimap trim (trim . tail) $ splitAt idx str

trim, xprop :: ShowS
trim = dropWhileEnd isSpace . dropWhile isSpace
xprop = unsafeDupablePerformIO . xProperty
