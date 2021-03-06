import XMonad
import XMonad.Config.Desktop

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Util.Run

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.NoBorders

main :: IO ()
main = do
    xmproc <- spawnPipe "picom"
    xmproc <- spawnPipe "sleep 2 && trayer --edge top --align center --width 5 --height 26 --distancefrom left --distance 300 --transparent true --alpha 0 --tint 0x0B2422"
    xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

--
-- General Configs
--
myConfig = desktopConfig
    { terminal = "alacritty"
    , modMask = mod4Mask
    , normalBorderColor = "#0B2422"
    , focusedBorderColor = "#51A59D"
    , layoutHook = (lessBorders OnlyFloat) myLayoutHook
    }
    `additionalKeysP` myKeymaps

--
-- Keymaps
--
myKeymaps :: [ ([Char], X()) ]
myKeymaps =
    [ ("s-p", spawn "dmenu_run -i -p 'Type to search' -fn 'FiraCode-10' -nb '#0B2422' -nf '#E9FFFA' -sb '#2A6F69'")
    , ("<Print>", spawn "flameshot gui")
    , ("<XF86MonBrightnessUp>", spawn "light -A 10")
    , ("C-<XF86MonBrightnessUp>", spawn "light -A 1")
    , ("<XF86MonBrightnessDown>", spawn "light -U 10")
    , ("C-<XF86MonBrightnessDown>", spawn "light -U 1")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume $(pactl get-default-sink) +5%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume $(pactl get-default-sink) -5%")
    ]

--
-- Layout
--
myLayoutHook = tiled ||| noBorders Full ||| Mirror tiled
    where tiled = Tall 1 (3/100) (3/5)

--
-- Formatting messages to xmobar
--
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = "    "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#51A59D" 2
    , ppHidden          = bgLight . wrap " " ""
    , ppUrgent          = fgPrimary . wrap (fgPrimary "!") (fgPrimary "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = fgPrimary . ppWindow
    formatUnfocused = bgLight . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30


    -- Colors
    bgDarker, bgDark, bgLight, fgWhite, fgPrimary :: String -> String
    bgDarker  = xmobarColor "#0B2422" ""
    bgDark    = xmobarColor "#1E5954" ""
    bgLight   = xmobarColor "#51A59D" ""
    fgWhite   = xmobarColor "#E9FFFA" ""
    fgPrimary = xmobarColor "#51A59D" ""

