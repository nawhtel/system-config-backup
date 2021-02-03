import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Time

import Graphics.X11.ExtraTypes.XF86

import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.TwoPanePersistent
import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Drawer

import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import XMonad.Actions.WithAll
import XMonad.Actions.Promote
import XMonad.Actions.MouseGestures
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_p     ), spawn "passmenu --type")
    , ((modm,               xK_b     ), gotoMenu)
    , ((modm .|. shiftMask, xK_b     ), bringMenu)
    , ((modm, xK_g), goToSelected def)

    , ((modm,  xK_w), spawn "chromium")
    , ((modm,  xK_z), spawn "zathura")
    , ((modm,  xK_r), spawn "rox")
    , ((modm,  xK_e), spawn "emacsclient -a \"\" -c")
    , ((modm,  xK_d), spawn "emacsclient -a \"\" -c -e \"(dired \\\"~/\\\")\"")
    , ((modm,  xK_s), spawn "emacsclient -a \"\" -c -e \"(vterm)\"")

    , ((modm .|. shiftMask, xK_t), sinkAll)

    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 2+")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 2-")
    , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_o   ), windows W.focusUp)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp)
    , ((modm,               xK_m     ), windows W.focusMaster)
    , ((modm,               xK_Return), promote)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm .|. shiftMask, xK_h     ), sendMessage ShrinkSlave)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_l     ), sendMessage ExpandSlave)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    -- ++
    -- -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3

    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
gestures = M.fromList
  [ ([], focus)
  , ([U], \w -> focus w >> windows W.swapUp)
  , ([D], \w -> focus w >> windows W.swapDown)
  , ([R, D], \_ -> sendMessage NextLayout)
  ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    , ((modm .|. shiftMask, button3), mouseGesture gestures)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

myLayout = lessBorders (Combine Difference Screen OnlyScreenFloat) $
           spacingRaw True (Border 10 10 5 10) True (Border 2 2 2 2) True $
           simpleDrawer 0.003 0.4 drawApps `onRight` tiled
           ||| tabbed shrinkText def { decoHeight = 15 }
           ||| TwoPanePersistent Nothing (3/100) (1/2)
  where drawApps = ClassName "XTerm" `Or` ClassName "Xchat"
        tiled = mouseResizableTile{ masterFrac = 0.5, draggerType = FixedDragger 8 8}

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = handleEventHook def <+> fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

boogieWallpaper :: IO String
boogieWallpaper = wallToPath . daytWall . dayOfWeek . utctDay <$> getCurrentTime
  where wallToPath s = "~/Pictures/wallpapers/" ++ s
        daytWall dow = case dow of
          Monday -> "kde.jpg"
          Tuesday -> "kde.png"
          Wednesday -> "kde2.png"
          Thursday -> "gentoo.jpg"
          Friday -> "kde3.png"
          Saturday -> "kde4.png"
          Sunday ->  "kde5.png"

myStartupHook = do
  wallpaperPath <- liftIO boogieWallpaper
  spawn $ "feh --bg-scale " ++ wallpaperPath
  setDefaultCursor xC_left_ptr
  spawnOnce "emacs --daemon &"
  spawnOnce "compton -cf -t-9 -l-11 -r9 -o.95 -I 0.028 -D 5 &"
  spawnOnce "xset r rate 250 30"
  spawnOnce "xautolock -time 30 -locker \"sudo loginctl suspend\" -detectsleep"

main = xmonad $ ewmh def {
    -- simple stuff
    terminal           = "xterm",
    focusFollowsMouse  = True,
    clickJustFocuses   = False,
    modMask            = mod4Mask,
    workspaces         = ["1","2","3","4","5","6","7","8","9"],
    borderWidth        = 1,
    normalBorderColor  = "gray",
    focusedBorderColor = "#116688",
    
    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    -- hooks, layouts
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    logHook            = myLogHook,
    startupHook        = myStartupHook
    }
