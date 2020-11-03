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

-- Layout
import Layouts

-- Actions
import XMonad.Actions.SpawnOn
import XMonad.Actions.PhysicalScreens(viewScreen, sendToScreen, horizontalScreenOrderer)
import XMonad.Actions.CopyWindow(kill1)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main = do
    xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/.xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/.xmobarrc"
    myTerminal <- getEnv "TERMINAL"
    xmonad $ defaultConfig {
        -- startupHook = myStartupHook,

        -- Adds support for a status bar and dock
        manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,
        layoutHook = myLayoutHook,
        -- layoutHook = avoidStruts $ layoutHook defaultConfig,
        -- this must be in this order, docksEventHook must be last
        handleEventHook = handleEventHook defaultConfig <+> docksEventHook,
        logHook = dynamicLogWithPP xmobarPP {
                        ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x,
                        ppTitle = xmobarColor "green" "" . shorten 50
                        },

        terminal = myTerminal,
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

myManageHook = composeAll [
    className =? "TelegramDesktop" --> doFloat,
    (isFullscreen --> doFullFloat)
    ]

-------------------------------------------------------------------------------
-- Layout Hook
-------------------------------------------------------------------------------

enabledLayouts = tall ||| monocle

myLayoutHook = avoidStruts $ enabledLayouts

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

