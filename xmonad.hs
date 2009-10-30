import XMonad

-- Dzen imports {{{
import IO
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run              (spawnPipe)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
-- }}}

-- Layout imports {{{
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints    (layoutHintsWithPlacement)
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace   (onWorkspace)
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.ManageHelpers

import Data.Ratio ((%))
-- }}}

main = do
	d <- spawnPipe "dzen2 -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*' -bg '#1a1a1a' -fg '#aaaaaa' -h 14 -w 500 -ta l "
	spawn "~/bin/dzconky"
	xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
		{ terminal    = myTerminal
		, modMask     = myModMask
		, borderWidth = myBorderWidth
		, normalBorderColor  = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor
		, workspaces  = myWorkspaces
		, logHook     = myLogHook d
		, layoutHook  = myLayoutHook
		, manageHook  = myManageHook
		}

myTerminal    = "urxvt"
myModMask     = mod4Mask
myBorderWidth = 1
myNormalBorderColor = "#303030"
myFocusedBorderColor = "#ff0000"
myWorkspaces  = ["code", "web", "chat", "download", "gimp", "misc"]
myLogHook h   = dynamicLogWithPP $ defaultPP
	{ ppCurrent = dzenColor "red" "" . wrap "[" "] "
	, ppVisible = wrap "[" "] "
	, ppHidden = dzenColor "grey" "" . wrap "" " "
	, ppHiddenNoWindows = dzenColor "grey" "" . wrap "" " "
	, ppUrgent = dzenColor "blue" "" . wrap "^" ""
	, ppLayout = dzenColor "grey" ""
	, ppTitle = const ""
	, ppOutput = hPutStrLn h 
	}
myLayoutHook  = avoidStruts $ onWorkspace "chat" chatLayout $ standardLayouts
	where
		standardLayouts = tiled ||| Mirror tiled ||| full
		chatLayout      = withIM (1%8) chatProp standardLayouts
		chatProp        = Role "buddy_list"

		tiled           = hinted (ResizableTall nmaster delta ratio [])
		full            = hinted (noBorders Full)

		hinted l        = layoutHintsWithPlacement (0,0) l

		nmaster         = 1
		delta           = 3/100
		ratio           = toRational (2/(1 + sqrt 5 :: Double)) -- golden ratio
myManageHook  = (composeAll . concat $
	[ [isFullscreen             --> doFullFloat ]
	]) <+> manageDocks
