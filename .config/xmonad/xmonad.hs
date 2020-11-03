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
import XMonad.Hooks.ManageHelpers(isFullscreen,doFullFloat)

-- Actions
import XMonad.Actions.SpawnOn
import XMonad.Actions.PhysicalScreens(viewScreen, sendToScreen, horizontalScreenOrderer)
import XMonad.Actions.CopyWindow(kill1)

import Layouts

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main = do
    (bar0, bar1) <- spawnStatusBars
    importantEnv <- genImportantEnv
    xmonad $ defaultConfig {
        -- startupHook = myStartupHook,
        manageHook = myManageHook,
        layoutHook = myLayoutHook,
        -- this must be in this order, docksEventHook must be last
        handleEventHook = handleEventHook defaultConfig <+> docksEventHook,
        logHook = myLogHook bar0 bar1,
        terminal = term importantEnv,
        modMask = myModKey,
        borderWidth = myBorderWidth,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor
        } `additionalKeysP` keybindings

-------------------------------------------------------------------------------
-- Basic Settings
-------------------------------------------------------------------------------

myModKey   = mod4Mask -- Super Key

-- Border settings
myBorderWidth        = 2
myNormalBorderColor  = "#292d3e"
myFocusedBorderColor = "#bbc5ff"

xmobarConfigFile = "~/.config/xmobar/.xmobarrc"

-------------------------------------------------------------------------------
-- Environment
-------------------------------------------------------------------------------

data ImportantEnvironment = ImportantEnvironment { term :: String }

genImportantEnv = do
    terminal <- getEnv "TERMINAL"
    return ImportantEnvironment { term = terminal }

-------------------------------------------------------------------------------
-- Status Bar
-------------------------------------------------------------------------------

spawnXmobar x = spawnPipe ("xmobar -x " ++ x ++ " " ++ xmobarConfigFile)

spawnStatusBars = do
    bar0 <- spawnXmobar "0"
    bar1 <- spawnXmobar "1"
    return (bar0, bar1)

-------------------------------------------------------------------------------
-- Startup Hook
-------------------------------------------------------------------------------

-- myStartupHook = do
    -- spawnOn "workspace1" myTerminal
    -- spawnOn "workspace2" "telegram-desktop"
    -- spawnOn "workspace3" myBrowser

-------------------------------------------------------------------------------
-- Manage Hook
-------------------------------------------------------------------------------

myManageHook = manageDocks <+> composeAll [
    className =? "TelegramDesktop" --> doFloat
    ]

-------------------------------------------------------------------------------
-- Layout Hook
-------------------------------------------------------------------------------

enabledLayouts = tall ||| monocle

myLayoutHook = avoidStruts $ enabledLayouts

-------------------------------------------------------------------------------
-- Log Hook
-------------------------------------------------------------------------------

myLogHook bar0 bar1 = dynamicLogWithPP xmobarPP {
    ppOutput = \x -> hPutStrLn bar0 x >> hPutStrLn bar1 x,
    ppTitle = xmobarColor "green" "" . shorten 50
    }

-------------------------------------------------------------------------------
-- Keybindings
-------------------------------------------------------------------------------

keybindings = [
    -- XMonad
    ("M-S-r", spawn "xmonad --restart"),

    -- Windows
    ("M-S-e", kill1),

    -- Windows navigation
    ("M-m", windows W.focusMaster),
    ("M-j", windows W.focusDown),
    ("M-k", windows W.focusUp),
    ("M-S-m", windows W.swapMaster),
    ("M-S-j", windows W.swapDown),
    ("M-S-k", windows W.swapUp),

    -- Screen navigation
    ("M-<F1>", viewScreen horizontalScreenOrderer 0),
    ("M-<F2>", viewScreen horizontalScreenOrderer 1),
    ("M-<F3>", viewScreen horizontalScreenOrderer 2),
    ("M-S-<F1>", sendToScreen horizontalScreenOrderer 0),
    ("M-S-<F2>", sendToScreen horizontalScreenOrderer 1),
    ("M-S-<F3>", sendToScreen horizontalScreenOrderer 2),

    -- Layouts
    ("M-S-<Space>", sendMessage ToggleStruts),

    -- Spawning programs
    ("M-<Return>", spawn "$TERMINAL"),
    ("M-r", spawn "dmenu_run"), -- application launcher
    ("M-/", spawn "lockscreen"),
    ("M-S-b", spawn "$BROWSER"),

    -- Multimedia
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute"),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    ]

