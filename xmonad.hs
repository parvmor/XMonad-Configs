import Data.Default

import System.Exit
import System.IO

import XMonad hiding ((|||))

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Actions.GroupNavigation
import XMonad.Actions.PhysicalScreens

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Fullscreen
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleDecoration(shrinkText)
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Dwindle
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane

import XMonad.Util.EZConfig(additionalKeys, additionalKeysP, additionalMouseBindings)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare


myTabConfig = def { activeColor = "#556064"
                  , activeTextColor = "#80FFF9"
                  , activeBorderColor = "#454948"
                  , inactiveColor = "#2F3D44"
                  , inactiveTextColor = "#1ABC9C"
                  , inactiveBorderColor = "#454948"
                  , urgentColor = "#FDF6E3"
                  , urgentTextColor = "#1ABC9C"
                  , urgentBorderColor = "#268BD2"
                  , fontName = "xft:Noto Sans CJK:size=10:antialias=true"
                  }

myLayout = avoidStruts $
  -- noBorders (fullscreenFull Full)
  noBorders Full
  ||| noBorders (tabbed shrinkText myTabConfig)
  ||| tiled
  ||| Mirror tiled
  ||| twopane
  ||| Mirror twopane
  ||| ThreeColMid 1 (3/100) (1/2)
  ||| emptyBSP
  ||| Spiral L XMonad.Layout.Dwindle.CW (3/2) (11/10)
  where
     -- The last parameter is fraction to multiply the slave window heights
     -- with. Useless here.
     tiled = spacing 3 $ ResizableTall nmaster delta ratio []
	 -- In this layout the second pane will only show the focused window.
     twopane = spacing 3 $ TwoPane delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myPP = def { ppCurrent = xmobarColor "#1ABC9C" "" . wrap "[" "]"
           , ppTitle = xmobarColor "#1ABC9C" "" . shorten 60
           , ppVisible = wrap "(" ")"
           , ppUrgent  = xmobarColor "red" "yellow"
           , ppSort = getSortByXineramaPhysicalRule def
           }

myManageHook = composeAll [ isFullscreen --> doFullFloat ]

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- lock the screen
    , ((modMask .|.  shiftMask, xK_l), spawn "/usr/bin/xscreensaver-command --lock")
    -- close the focused window
    , ((modMask .|. shiftMask, xK_c), kill)
    -- destroy the entire session
    , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    -- Rotate through the next layout
    , ((modMask, xK_space), sendMessage NextLayout)
    -- Reste the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    -- Change the layout to fullscreen
    , ((modMask .|. shiftMask, xK_f), sendMessage $ JumpToLayout "Full")
    -- Resize viewed windows to the correct size
    , ((modMask, xK_n), refresh)
    -- Move foucs to the next window
    , ((modMask, xK_Tab), windows W.focusDown)
    -- Move focus to the previous window
    , ((modMask .|. shiftMask, xK_Tab), windows W.focusUp)
    -- Move focus to the master window
    , ((modMask, xK_m), windows W.focusMaster)
    -- Swap the focused window with the master window
    , ((modMask, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink the master window
    , ((modMask, xK_h), sendMessage Shrink)
    -- Expand the master window
    , ((modMask, xK_l), sendMessage Expand)
    -- Shrink ratio between secondary panes for ResizeableTall layout
    , ((modMask .|. shiftMask, xK_h), sendMessage MirrorShrink)
    -- Expand ratio between secondary panes for ResizeableTall layout
    , ((modMask .|. shiftMask, xK_h), sendMessage MirrorExpand)
    -- Push window back into tiling
    , ((modMask, xK_t), withFocused $ windows . W.sink)
    -- Increase number of window in master area
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))
    -- Decrease number of window in master area
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
    -- Recompile xmonad config and refresh the setup
    , ((modMask, xK_q), restart "xmonad" True)
    -- Toggles the status bar gap
    , ((modMask, xK_b), sendMessage ToggleStruts)
    -- Take a screenshot
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
	-- Open rofi in combi mode
    , ((modMask, xK_p), spawn "rofi -combi-modi window,run,drun -show combi -modi combi")
    ] ++
    -- Sends the focused window to the given workspace
    [ ((m .|. modMask, k), windows $ f i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9], (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ] ++
    -- Switches between physical screens
    [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f)) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..], (f, m) <- [(W.view, 0), (W.shift, shiftMask)] ] ++
    -- Swaps the current workspace with the given workspace
    [ ((modMask .|. controlMask, k), windows $ swapWithCurrent i) | (i, k) <- zip myWorkspaces [xK_1 .. xK_9] ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Take the focused window from tiling
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- Swap the focused window with master
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- Resize the focused window with mouse
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh def
        { modMask = mod4Mask
        , keys = myKeys
        , mouseBindings = myMouseBindings
        , focusFollowsMouse = True
        , terminal = "/home/parv/.local/kitty.app/bin/kitty"
        , manageHook = manageDocks <+> myManageHook
        , layoutHook = myLayout
        , handleEventHook = handleEventHook def <+> docksEventHook
        , logHook = dynamicLogWithPP myPP { ppOutput = hPutStrLn xmproc } >> historyHook
        , normalBorderColor = "#2F3D44"
        , focusedBorderColor = "#1ABC9C"
        } `additionalKeysP`
        [ ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
        , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
		, ("<XF86AudioMute>", spawn "amixer -q set Master togglemute")
        , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 -q set Master 2dB-")
        , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 -q set Master 2dB+")
        , ("<XF86AudioPlay>", spawn "playerctl play-pause")
        , ("<XF86AudioNext>", spawn "playerctl next")
        , ("<XF86AudioPrev>", spawn "playerctl prev")
        ]
