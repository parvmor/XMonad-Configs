import qualified Data.Map as M
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.SwapWorkspaces (swapWithCurrent)
import           XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as E
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.EZConfig(additionalKeys)
import           XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W

myTerminal    = "/usr/bin/gnome-terminal"
myWorkspaces  = ["1:web", "2:code", "3:SoH(Haskell)", "4:Tipsy(Scala)", "5:Competitive"] ++ map show [6..9]
myScreensaver = "/usr/bin/xscreensaver-command --lock"
myLauncher    = "$(yeganesh -x -- -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"

myManageHook = composeAll
    [ className =? "chromium"    --> doShift "1:web"
    , className =? "VirtualBox"  --> doShift "3:vm"
    , className =? "stalonetray" --> doIgnore
    , className =? "vlc"         --> doShift "four"
    , className =? "skype"       --> doShift "four"
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ]

myLayout = avoidStruts (
    ThreeColMid 1 (3/100) (1/2) |||
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"
myBorderWidth        = 2

tabConfig = defaultTheme {
    activeBorderColor   = "#7c7c7c",
    activeTextColor     = "#ceffac",
    activeColor         = "#000000",
    inactiveBorderColor = "#7c7c7c",
    inactiveTextColor   = "#eeeeee",
    inactiveColor       = "#000000"
}

xmobarTitleColor            = "#ffb6b0"
xmobarCurrentWorkspaceColor = "#ceffac"

myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|.  shiftMask, xK_l), spawn myScreensaver)
    , ((modMask, xK_p), spawn myLauncher)
--    , ((modMask .|. shiftMask, xK_p), spawn "select-screenshot")
--    , ((modMask .|. controlMask .|. shiftMask, xK_p), spawn "screenshot")
--
    , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")
    , ((0, xF86XK_MonBrightnessUp), spawn "~/.xmonad/bin/brightness +2")
    , ((0, xF86XK_MonBrightnessDown), spawn "~/.xmonad/bin/brightness -2")
--    , ((0, 0x1008FF2C), spawn "eject -T")
--
    , ((modMask .|. shiftMask, xK_c), kill)
    , ((modMask, xK_space), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_n), refresh)
    , ((modMask, xK_Tab), windows W.focusDown)
    , ((modMask, xK_j), windows W.focusDown)
    , ((modMask, xK_k), windows W.focusUp)
    , ((modMask, xK_m), windows W.focusMaster)
    , ((modMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)
    , ((modMask, xK_h), sendMessage Shrink)
    , ((modMask, xK_l), sendMessage Expand)
    , ((modMask, xK_t), withFocused $ windows . W.sink)
    , ((modMask, xK_comma), sendMessage (IncMasterN 1))
    , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    , ((modMask, xK_q), restart "xmonad" True)
    ] ++
    [ ((m .|. modMask, k), windows $ f i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9], (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ] ++
    [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f)) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..], (f, m) <- [(W.view, 0), (W.shift, shiftMask)] ] ++
    [ ((modMask .|. controlMask, k), windows $ swapWithCurrent i) | (i, k) <- zip myWorkspaces [xK_1 .. xK_9] ]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myStartupHook = do
    E.ewmhDesktopsStartup
    return ()

--myEventHook = E.ewmhDesktopsEventHook <+> E.fullscreenEventHook <+> fullscreenEventHook
--myEventHook = E.ewmhDesktopsEventHook

main = do
    xmproc <- spawnPipe "/usr/local/bin/xmobar ~/.xmobarrc"
    xmonad $ defaults
        { manageHook      = manageDocks <+> manageHook defaultConfig
        , handleEventHook = handleEventHook defaultConfig <+> E.ewmhDesktopsEventHook <+> E.fullscreenEventHook <+> docksEventHook
        , logHook         = dynamicLogWithPP $ xmobarPP
            { ppOutput  = hPutStrLn xmproc
            , ppTitle   = xmobarColor xmobarTitleColor "" . shorten 20
            , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
            , ppSep     = "  "
            }
        }


defaults = E.ewmh $ defaultConfig
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , layoutHook         = smartBorders $ myLayout
    , manageHook         = myManageHook
--    , handleEventHook    = myEventHook
    , startupHook        = myStartupHook
    }
