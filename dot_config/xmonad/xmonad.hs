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
import XMonad.Layout.Spacing

main :: IO ()
main = do
    xmproc <- spawnPipe "picom"  -- X11 Compositor
    xmproc <- spawnPipe "sleep 2 && trayer --edge top --align right --width 10 --height 35 --distancefrom right --distance 450 --transparent true --alpha 0 --tint 0x0B2422"
    xmproc <- spawnPipe "wired"  -- Notification manager
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
    , borderWidth = 2
    , focusedBorderColor = "#51A59D"
    , normalBorderColor = "#1E5954"
    , layoutHook = (lessBorders OnlyFloat) myLayoutHook
    }
    `additionalKeysP` myKeymaps

--
-- Keymaps
--
myKeymaps :: [ ([Char], X()) ]
myKeymaps =
    [ ("s-p", spawn "dmenu_run")
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
    where tiled = smartBorders $ spacingWithEdge 4 $ Tall 1 (3/100) (3/5)

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
    , ppOrder           = \[ws, _, _, wins] -> [ws, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = fgWhite . ppWindow
    formatUnfocused = bgDark . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "Untitled" else w) . shorten 50


    -- Colors
    bgDarker, bgDark, bgLight, fgWhite, fgPrimary :: String -> String
    bgDarker  = xmobarColor "#0B2422" ""
    bgDark    = xmobarColor "#1E5954" ""
    bgLight   = xmobarColor "#51A59D" ""
    fgWhite   = xmobarColor "#E9FFFA" ""
    fgPrimary = xmobarColor "#51A59D" ""
