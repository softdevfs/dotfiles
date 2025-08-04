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

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier

import XMonad.Prompt

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified XMonad.Util.Hacks as Hacks
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.Taffybar.Support.PagerHints (pagerHints)

myTerminal :: String
myTerminal      = "kitty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth   = 1

myModMask :: KeyMask
myModMask       = mod1Mask

myWorkspaces :: [String]
myWorkspaces = [" www ", " dev ", " sys ", " mus ", " chat ", " doc ", " vid ", " vbox ", " gfx "]

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

myNormalBorderColor  :: String
myNormalBorderColor  = "#ff0000"

myFocusedBorderColor :: String
myFocusedBorderColor = "#00e673"

amazonES :: SearchEngine
amazonES = searchEngine "amazonES" "https://www.amazon.es/s?k="

defaultXPConfig :: XPConfig
defaultXPConfig = def
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

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys = \c -> mkKeymap c $
  [
      ("M-S-<Return>", spawnOn " sys " myTerminal)
      , ("M-p", shellPromptHere defaultXPConfig)
      , ("M-a", promptSearch defaultXPConfig amazonES)
      , ("M-g", promptSearch defaultXPConfig google)
      , ("M-y", promptSearch defaultXPConfig youtube)
      , ("M-f", spawnOn " www " "firefox")
      , ("M-v", spawn "pamixer --increase 5")
      , ("M-S-v", spawn "pamixer --decrease 5")
      , ("M-e", spawn "~/scripts/emoji.sh")
      , ("M-s", spawn "scrot ~/pictures/screenshots/screenshot.png")
      , ("C-M-s", spawn "scrot -s ~/pictures/screenshots/screenshot.png")
      , ("C-M-c", (windows copyToAll))
      , ("C-M-S-c",  killAllOtherCopies)
      , ("M-S-c", kill)
      , ("M-r", spawn "killall loop_wp.py")
      , ("M-<Space>", sendMessage NextLayout)
      , ("M-n", refresh)
      , ("M-<Tab>", windows W.focusDown)
      , ("M-j", windows W.focusDown)
      , ("M-k", windows W.focusUp)
      , ("M-<Return>", windows W.swapMaster)
      , ("M-S-j", windows W.swapDown)
      , ("M-S-k", windows W.swapUp)
      , ("M-h", sendMessage Shrink)
      , ("M-l", sendMessage Expand)
      , ("M-t", withFocused $ windows . W.sink)
      , ("M-,", sendMessage (IncMasterN 1))
      , ("M-.", sendMessage (IncMasterN (-1)))
      , ("C-M-<Up>", withFocused $ keysMoveWindow (0, -20))
      , ("C-M-<Down>", withFocused $ keysMoveWindow (0, 20))
      , ("C-M-<Left>", withFocused $ keysMoveWindow (-20, 0))
      , ("C-M-<Right>", withFocused $ keysMoveWindow (20, 0))
      , ("C-M-S-<Up>", withFocused $ keysResizeWindow (0, -20) (0, 0))   
      , ("C-M-S-<Down>", withFocused $ keysResizeWindow (0, 20) (0, 0))
      , ("C-M-S-<Left>", withFocused $ keysResizeWindow (-20, 0) (0, 0))
      , ("C-M-S-<Right>", withFocused $ keysResizeWindow (20, 0) (0,0))
      , ("M-1", (windows $ W.greedyView $ myWorkspaces !! 0))
      , ("M-2", (windows $ W.greedyView $ myWorkspaces !! 1))
      , ("M-3", (windows $ W.greedyView $ myWorkspaces !! 2))
      , ("M-4", (windows $ W.greedyView $ myWorkspaces !! 3))
      , ("M-5", (windows $ W.greedyView $ myWorkspaces !! 4))
      , ("M-6", (windows $ W.greedyView $ myWorkspaces !! 5))
      , ("M-7", (windows $ W.greedyView $ myWorkspaces !! 6))
      , ("M-8", (windows $ W.greedyView $ myWorkspaces !! 7))
      , ("M-9", (windows $ W.greedyView $ myWorkspaces !! 8))
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

myMouseBindings :: XConfig l -> M.Map (ButtonMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                          >> windows W.shiftMaster))
      , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
      , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                            >> windows W.shiftMaster))
    ]


myShowWNameTheme :: SWNConfig 
myShowWNameTheme = def
  { swn_font              = "xft:Ubuntu:bold:size=40"
  , swn_fade              = 1.0
  , swn_bgcolor           = "#1c1f24"
  , swn_color             = "#ffffff"
  }


myLayout :: ModifiedLayout (AvoidStruts) (Choose Tall Full) a
myLayout = avoidStruts (tiled ||| Full)
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100
     
myManageHook :: ManageHook 
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ]

myEventHook :: Event -> X All
myEventHook = handleEventHook def <> Hacks.windowedFullscreenFixEventHook

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "~/scripts/loop_wp.py &"
    spawnOnce "taffybar &"
    spawnOnce "picom -b"

main :: IO ()
main = xmonad $ ewmh $ docks $ pagerHints $ def {
	terminal           = myTerminal,
	focusFollowsMouse  = myFocusFollowsMouse,
	clickJustFocuses   = myClickJustFocuses,
	borderWidth        = myBorderWidth,
	modMask            = myModMask,
	workspaces         = myWorkspaces,
	normalBorderColor  = myNormalBorderColor,
	focusedBorderColor = myFocusedBorderColor,
	keys               = myKeys,
	mouseBindings      = myMouseBindings,
	layoutHook         = showWName' myShowWNameTheme $ spacingWithEdge 5 $ myLayout,
	manageHook         = manageSpawn <> myManageHook,
	handleEventHook    = myEventHook,
	startupHook        = myStartupHook
}
