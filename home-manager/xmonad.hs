import XMonad
import XMonad.Config.Desktop

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Util.Run

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive        ( fadeInactiveLogHook )
import qualified Codec.Binary.UTF8.String              as UTF8
import qualified DBus                                  as D
import qualified DBus.Client                           as D

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.SimplestFloat

main :: IO ()
main = mkDbusClient >>= main'

main' dbus = do
    xmonad
     . docks
     . ewmhFullscreen
     . ewmh
     $ myConfig dbus

--
-- General Configs
--
myConfig dbus = desktopConfig
    { terminal           = "alacritty"
    , modMask            = mod4Mask
    , borderWidth        = 2
    , focusedBorderColor = "#51A59D"
    , normalBorderColor  = "#1E5954"
    , layoutHook         = (lessBorders OnlyFloat) myLayoutHook
    , logHook            = myPolybarLogHook dbus
    }
    `additionalKeysP` myKeymaps

--
-- Keymaps
--
myKeymaps :: [ ([Char], X()) ]
myKeymaps =
    [ ("M4-p", spawn "rofi -show drun")
    , ("<Print>", spawn "flameshot gui")
    , ("<XF86AudioRaiseVolume>", spawn "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 10%+")
    , ("<XF86AudioLowerVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 10%-")
    , ("<XF86AudioMute>", spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
    , ("S-<XF86AudioLowerVolume>", spawn "brightnessctl -n set 5%-")
    , ("S-<XF86AudioRaiseVolume>", spawn "brightnessctl -n set +5%")
    ]

--
-- Layout
--
myLayoutHook = avoidStruts (tiled) ||| noBorders Full ||| avoidStruts (Mirror tiled) ||| simplestFloat
    where tiled = smartBorders
            $ spacingWithEdge 4 $ Tall 1 (3/100) (3/5)

--
-- Dbus signals to polybar
--
mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body   = [D.toVariant $ UTF8.decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      darkGreen  = "#0B2422"
      green      = "#1E5954"
      lightGreen = "#51A59D"
      white      = "#E9FFFA"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper lightGreen
          , ppUrgent          = wrapper white
          , ppHidden          = wrapper green
          , ppTitle           = wrapper lightGreen . shorten 90
          , ppSep             = "   "
          }

myPolybarLogHook dbus = myLogHook <+> dynamicLogWithPP (polybarHook dbus)

myLogHook = fadeInactiveLogHook 0.9
