------------------------------------------------------------------------------------------------
--  m    m m    m                          #                                m""    "          --
--   #  #  ##  ##  mmm   m mm    mmm    mmm#          mmm    mmm   m mm   mm#mm  mmm     mmmm --
--    ##   # ## # #" "#  #"  #  "   #  #" "#         #"  "  #" "#  #"  #    #      #    #" "# --
--   m""m  # "" # #   #  #   #  m"""#  #   #         #      #   #  #   #    #      #    #   # --
--  m"  "m #    # "#m#"  #   #  "mm"#  "#m##         "#mm"  "#m#"  #   #    #    mm#mm  "#m"# --
--                                                                                       m  # --
--                                                                                        ""  --
--      By: B34n5                                                                             --
------------------------------------------------------------------------------------------------

import XMonad

import System.Exit

import Data.Char
import Data.Maybe (fromJust)
import Data.Monoid

import XMonad.Actions.CopyWindow
import XMonad.Actions.FloatKeys
import XMonad.Actions.Search
import XMonad.Actions.SpawnOn

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing

import XMonad.Prompt

import XMonad.Util.EZConfig (mkKeymap, additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified XMonad.Util.Hacks as Hacks
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


-- VARIABLES
--------------------------------------------------------------------------

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]

myWorkspaces = [" www ", " dev ", " sys ", " mus ", " chat ", " doc ", " vid ", " vbox ", " gfx "]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key alt+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices


-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#ff0000"
myFocusedBorderColor = "#00e673"

-- search engine

amazonES = searchEngine "amazonES" "https://www.amazon.es/s?k="

-- XPConfig (default)

defaultXPConfig = def
-- XPC
  {
    font = "xft:JetBrains Mono:size=12",
    bgColor = "#282828",
    fgColor = "#458588",
    promptBorderWidth = 0,
    position = Top,
    alwaysHighlight = True,
    height = 30,
    historySize = 5
  }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys = \c -> mkKeymap c $
  [
      -- launch terminal
      ("M-S-<Return>", spawnOn " sys " myTerminal)
      -- launch prompt
      , ("M-p", shellPromptHere defaultXPConfig)
      -- search in amazon
      , ("M-a", promptSearch defaultXPConfig amazonES)
      -- search in google
      , ("M-g", promptSearch defaultXPConfig google)
      -- search in youtube
      , ("M-y", promptSearch defaultXPConfig youtube)
      -- launch emacs
      , ("M-c", spawnOn " dev " "emacs")
      -- launch firefox
      , ("M-f", spawnOn " www " "firefox-esr")
      -- launch weechat
      , ("M-i", spawnOn " chat " "kitty irssi")
      -- set volume
      , ("M-v", spawn "pamixer --increase 5")
      , ("M-S-v", spawn "pamixer --decrease 5")
      -- launch scrot (screenshoots)
      , ("M-s", spawn "scrot ~/pictures/screenshots/screenshot.png")
      -- launch scrot -s (selected frame screenshoot)
      , ("C-M-s", spawn "scrot -s ~/pictures/screenshots/screenshot.png")
      -- record screen
      , ("M-S-s", spawnOn " sys " "kitty screenRecorder")
      -- play youtube video in mpv player
      , ("M-C-p", spawnOn " vid " "~/scripts/playYoutube")
      -- copy windows into all workspaces
      , ("C-M-c", (windows copyToAll))
      -- remove windows from all workspaces except current workspace
      , ("C-M-S-c",  killAllOtherCopies)
      -- close focused window
      , ("M-S-c", kill)
      -- kill loop_wp script process
      , ("M-r", spawn "killall loop_wp.py")
      -- Rotate through the available layout algorithms
      , ("M-<Space>", sendMessage NextLayout)
      -- Resize viewed windows to the correct size
      , ("M-n", refresh)
      -- Move focus to the next window
      , ("M-<Tab>", windows W.focusDown)
      -- Move focus to the next window
      , ("M-j", windows W.focusDown)
      -- Move focus to the previous window
      , ("M-k", windows W.focusUp)
      -- Move focus to the master window
      , ("M-m", windows W.focusMaster)
      -- Swap the focused window and the master window
      , ("M-<Return>", windows W.swapMaster)
      -- Swap the focused window with the next window
      , ("M-S-j", windows W.swapDown)
      -- Swap the focused window with the previous window
      , ("M-S-k", windows W.swapUp)
      -- Shrink the master area
      , ("M-h", sendMessage Shrink)
      -- Expand the master area
      , ("M-l", sendMessage Expand)
      -- Push window back into tiling
      , ("M-t", withFocused $ windows . W.sink)
      -- Increment the number of windows in the master area
      , ("M-,", sendMessage (IncMasterN 1))
      -- Deincrement the number of windows in the master area
      , ("M-.", sendMessage (IncMasterN (-1)))
      -- Quit xmonad
      , ("M-S-q", io (exitWith ExitSuccess))
      -- Restart xmonad
      , ("M-q", spawn "xmonad --recompile; xmonad --restart")
      -- Run xmessage with a summary of the default keybindings (useful for beginners)
      , ("M-<F1>", spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
      -- move floating windows
      , ("C-M-<Up>", withFocused $ keysMoveWindow (0, -20))
      , ("C-M-<Down>", withFocused $ keysMoveWindow (0, 20))
      , ("C-M-<Left>", withFocused $ keysMoveWindow (-20, 0))
      , ("C-M-<Right>", withFocused $ keysMoveWindow (20, 0))
      -- resize floating windows
      , ("C-M-S-<Up>", withFocused $ keysResizeWindow (0, -20) (0, 0))   
      , ("C-M-S-<Down>", withFocused $ keysResizeWindow (0, 20) (0, 0))
      , ("C-M-S-<Left>", withFocused $ keysResizeWindow (-20, 0) (0, 0))
      , ("C-M-S-<Right>", withFocused $ keysResizeWindow (20, 0) (0,0))
      --
      -- mod-[1..9], Switch to workspace N
      --
      , ("M-1", (windows $ W.greedyView $ myWorkspaces !! 0))
      , ("M-2", (windows $ W.greedyView $ myWorkspaces !! 1))
      , ("M-3", (windows $ W.greedyView $ myWorkspaces !! 2))
      , ("M-4", (windows $ W.greedyView $ myWorkspaces !! 3))
      , ("M-5", (windows $ W.greedyView $ myWorkspaces !! 4))
      , ("M-6", (windows $ W.greedyView $ myWorkspaces !! 5))
      , ("M-7", (windows $ W.greedyView $ myWorkspaces !! 6))
      , ("M-8", (windows $ W.greedyView $ myWorkspaces !! 7))
      , ("M-9", (windows $ W.greedyView $ myWorkspaces !! 8))
      --
      -- mod-shift-[1..9], Move client to workspace N
      --
      , ("M-S-1", (windows $ W.shift $ myWorkspaces !! 0))
      , ("M-S-2", (windows $ W.shift $ myWorkspaces !! 1))
      , ("M-S-3", (windows $ W.shift $ myWorkspaces !! 2))
      , ("M-S-4", (windows $ W.shift $ myWorkspaces !! 3))
      , ("M-S-5", (windows $ W.shift $ myWorkspaces !! 4))
      , ("M-S-6", (windows $ W.shift $ myWorkspaces !! 5))
      , ("M-S-7", (windows $ W.shift $ myWorkspaces !! 6))
      , ("M-S-8", (windows $ W.shift $ myWorkspaces !! 7))
      , ("M-S-9", (windows $ W.shift $ myWorkspaces !! 8))
  ]

alternativeModKey = mod4Mask
myExtendedKeysXConfig = def { modMask = alternativeModKey }

myExtendedKeys conf@(myExtendedKeysXConfig) = M.fromList $
  [
    ((alternativeModKey, xK_q), spawn "~/scripts/switch-qtile"),
    ((alternativeModKey, xK_m), spawn "~/scripts/switch-xmonad")
  ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    
    [
      -- mod-button1, Set the window to floating mode and move by dragging
      ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                          >> windows W.shiftMaster))

      -- mod-button2, Raise the window to the top of the stack
      , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

      -- mod-button3, Set the window to floating mode and resize by dragging
      , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                            >> windows W.shiftMaster))
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


------------------------------------------------------------------------

myShowWNameTheme = def
  { swn_font              = "xft:Ubuntu:bold:size=40"
  , swn_fade              = 1.0
  , swn_bgcolor           = "#1c1f24"
  , swn_color             = "#ffffff"
  }


------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = spacingWithEdge 5 $ avoidStruts (tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

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
    , resource  =? "kdesktop"       --> doIgnore
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook = mempty

myEventHook = handleEventHook def <> Hacks.windowedFullscreenFixEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()
myLogHook xmproc0 = dynamicLogWithPP $  filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc0 x   -- xmobar on monitor 1
        , ppCurrent = xmobarColor "#b16286" "" . wrap
                      ("<box type=Bottom width=2 mb=2 color=#b16286>") "</box>"
          -- Visible but not current workspace
        , ppVisible = xmobarColor "#b16286" "" . clickable
          -- Hidden workspace
        , ppHidden = xmobarColor "#458588" "" . wrap
                     ("<box type=Top width=2 mt=2 color=#458588>") "</box>" . clickable
          -- Hidden workspaces (no windows)
        , ppHiddenNoWindows = xmobarColor "#458588" ""  . clickable
          -- Title of active window
        , ppTitle = xmobarColor "#b16286" "" . shorten 60
          -- Separator character
        , ppSep =  "<fc=#928374> <fn=1>|</fn> </fc>"
          -- Urgent workspace
        , ppUrgent = xmobarColor "#cc241d" "" . wrap "!" "!"
          -- order of things in xmobar
        , ppOrder = \(ws:l:t:ex) -> [ws, t]
        }


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
	spawnOnce "~/scripts/loop_wp.py &"
	spawnOnce "setxkbmap es"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
	xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobar.config"
 	( xmonad =<< defaults xmproc ) $ ewmh $ docks
	-- xmonad $ ewmh $ docks $ defaults xmproc

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults xmproc = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys <> myExtendedKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = showWName' myShowWNameTheme $ myLayout,
        manageHook         = manageSpawn <> myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook xmproc,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch kitty",
    "mod-p            Launch prompt shell",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- move or resize floating windows",
    "M-C-{rightArrow|leftArrow}         Move window in the X axis",
    "M-C-{upArrow|downArrow}            Move window in the Y axis",
    "M-C-S-{rightArrow|leftArrow}   Resize window in the X axis",
    "M-C-S-{upArrow|downArrow}      Resize window in the Y axis",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
