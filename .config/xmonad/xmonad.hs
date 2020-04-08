import XMonad
import System.IO
import qualified XMonad.StackSet as W

-- Util
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Layout
import Xmonad.Layout.NoBorders

-- Actions
import XMonad.Actions.SpawnOn
import XMonad.Actions.CopyWindow (kill1)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main = do
    xmproc <- spawnPipe "xmobar ~/.config/xmobar/.xmobarrc"
    xmonad $ defaultConfig {
        -- startupHook = myStartupHook,

        -- Adds support for a status bar and dock
        manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,
        layoutHook = avoidStruts $ layoutHook defaultConfig,
        -- this must be in this order, docksEventHook must be last
        handleEventHook = handleEventHook defaultConfig <+> docksEventHook,
        logHook = dynamicLogWithPP xmobarPP {
                        ppOutput = hPutStrLn xmproc,
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

-- Programs
myTerminal = "termite"
myBrowser  = "chromium"

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
    className =? "TelegramDesktop" --> doFloat
    ]

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

    -- Layouts
    ("M-S-<Space>", sendMessage ToggleStruts),

    -- Spawning programs
    ("M-<Return>", spawn myTerminal),
    ("M-r", spawn "dmenu_run"), -- application launcher
    ("M-S-b", spawn myBrowser),

    -- Multimedia
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute"),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    ]

