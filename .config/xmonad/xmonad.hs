import XMonad
import System.IO
import System.Environment (getEnv)
import qualified XMonad.StackSet as W
import Data.Default

-- Util
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Actions
import XMonad.Actions.SpawnOn
import XMonad.Actions.PhysicalScreens(viewScreen, sendToScreen, horizontalScreenOrderer)
import XMonad.Actions.CopyWindow(kill1)

import XMonad.Layout.Fullscreen(fullscreenEventHook, fullscreenManageHook)
import XMonad.Layout.IndependentScreens

import XMonad.Hooks.EwmhDesktops(ewmh)

import Layouts

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main = do
    bars <- spawnStatusBars
    importantEnv <- genImportantEnv
    xmonad $ ewmh $ defaults importantEnv bars

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaults env bars = def {
    manageHook = myManageHook,
    layoutHook = myLayoutHook,
    -- this must be in this order, docksEventHook must be last
    handleEventHook = handleEventHook def
        <+> fullscreenEventHook
        <+> docksEventHook,
    logHook = myLogHook bars,
    workspaces = myWorkspaces,
    terminal = term env,
    modMask = myModKey,
    borderWidth = myBorderWidth,
    normalBorderColor = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor
    } `additionalKeysP` keybindings

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

myModKey   = mod4Mask -- Super Key

-- Border settings
myBorderWidth        = 2
myNormalBorderColor  = "#292d3e"
myFocusedBorderColor = "#bbc5ff"

myWorkspaces = (map show [1..9]) ++ myExtraWorkspaces
myExtraWorkspaces = ["0", "chat", "music", "movie"]

xmobarConfigFile = "~/.config/xmobar/.xmobarrc"

-------------------------------------------------------------------------------
-- Environment
-------------------------------------------------------------------------------

data ImportantEnvironment = ImportantEnvironment { term :: String
                                                 , browser :: String
                                                 }

genImportantEnv = do
    terminalVar <- getEnv "TERMINAL"
    browserVar <- getEnv "BROWSER"
    return ImportantEnvironment { term = terminalVar
                                , browser = browserVar
                                }

-------------------------------------------------------------------------------
-- Status Bar
-------------------------------------------------------------------------------

type StatusBar = Handle

spawnXmobar :: (MonadIO m) => String -> m StatusBar
spawnXmobar i = spawnPipe ("xmobar -x " ++ i ++ " " ++ xmobarConfigFile)

spawnStatusBars = do
    n <- countScreens
    bars <- mapM spawnXmobar (displays n)
    return bars
        where displays n = map show [0 .. n-1]

logToAllBars :: [StatusBar] -> String -> IO ()
logToAllBars bars msg = doLog bars
    where doLog (x:xs) = hPutStrLn x msg >> doLog xs
          doLog (x:_) = hPutStrLn x msg

-------------------------------------------------------------------------------
-- Hooks
-------------------------------------------------------------------------------

myManageHook = manageDocks <+> fullscreenManageHook

myLayoutHook = avoidStruts
    $ layouts
        where layouts = tall
                ||| monocle
                ||| wide

myLogHook bars = dynamicLogWithPP xmobarPP {
    ppTitle = fmtTitle,
    ppTitleSanitize = sanitTitle,
    ppCurrent = fmtCurrentWs,
    ppVisible = fmtVisibleWs,
    ppSep = "",
    ppWsSep = " ",
    ppOrder = order,
    ppOutput = logToAllBars bars
    }
        where fmtTitle t = xmobarColor white "" t
              sanitTitle t = " ~ " ++ (shorten 50 t)
              fmtWs bc id = xmobarColor white bc id
              fmtCurrentWs id = fmtWs wsBc ("[" ++ id ++ "]")
              fmtVisibleWs = fmtWs "black"
              order (ws:_:t:_) = [ws, t]
              white = "#e4ebed"
              wsBc = "#1a6078"

-------------------------------------------------------------------------------
-- Keybindings
-------------------------------------------------------------------------------

xmonadKeybindings = [
    ("M-S-r", spawn "killall xmobar; xmonad --restart")
    ]

windowKeybindings = [
    ("M-S-e", kill1)
    ]

windowNavigationKeybindings = [
    ("M-m", windows W.focusMaster),
    ("M-j", windows W.focusDown),
    ("M-k", windows W.focusUp),
    ("M-S-m", windows W.swapMaster),
    ("M-S-j", windows W.swapDown),
    ("M-S-k", windows W.swapUp)
    ]

screenNavigationKeybindings = [
    ("M-<F1>", viewScreen horizontalScreenOrderer 0),
    ("M-<F2>", viewScreen horizontalScreenOrderer 1),
    ("M-<F3>", viewScreen horizontalScreenOrderer 2),
    ("M-S-<F1>", sendToScreen horizontalScreenOrderer 0),
    ("M-S-<F2>", sendToScreen horizontalScreenOrderer 1),
    ("M-S-<F3>", sendToScreen horizontalScreenOrderer 2)
    ]

layoutKeybindings = [
    ("M-S-<Space>", sendMessage ToggleStruts)
    ]

programSpawningKeybindings = [
    ("M-<Return>", spawn "$TERMINAL"),
    ("M-r", spawn "dmenu_run"), -- application launcher
    ("M-/", spawn "lockscreen"),
    ("M-S-b", spawn "$BROWSER"),
    ("<Print>", spawn "tkscreen -s"),
    ("M-<Print>", spawn "tkscreen")
    ]

mediaKeybindings = [
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute"),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    ]

extraWorkspaceKeybinding = concat [ genMoveAndShiftToBinding k w | (k, w) <- keysAndWss ]
    where
        keysAndWss = [("0", "0"), ("c", "chat"), (",", "music"), ("v", "movie")]
        genMoveAndShiftToBinding key ws = [
            ("M-" ++ key, windows $ W.greedyView ws),
            ("M-S-" ++ key, windows $ W.shift ws)
            ]

keybindings = xmonadKeybindings
    ++ windowKeybindings
    ++ windowNavigationKeybindings
    ++ screenNavigationKeybindings
    ++ layoutKeybindings
    ++ programSpawningKeybindings
    ++ mediaKeybindings
    ++ extraWorkspaceKeybinding

