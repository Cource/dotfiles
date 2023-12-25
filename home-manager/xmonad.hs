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

import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus.Client              as DBusClient
import DBus

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.SimplestFloat

main :: IO ()
main = mkDbusClient >>= main'

main' dbus = do
    xmproc <- spawnPipe "eww open-many statusbar home"
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
    , logHook            = myLogHookWithSignal dbus
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
mkDbusClient :: IO DBusClient.Client
mkDbusClient = do
  dbus <- DBusClient.connectSession
  DBusClient.requestName dbus (DBus.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts =
    [ DBusClient.nameAllowReplacement
    , DBusClient.nameReplaceExisting
    , DBusClient.nameDoNotQueue
    ]

-- Emit a DBusBus signal on log updates
dbusOutput :: DBusClient.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = DBus.objectPath_ "/org/xmonad/Log"
      iname  = DBus.interfaceName_ "org.xmonad.Log"
      mname  = DBus.memberName_ "Update"
      signal = DBus.signal opath iname mname
      body   = [DBus.toVariant $ UTF8.decodeString str]
  in  DBusClient.emit dbus $ signal { DBus.signalBody = body }

-- myPpHook :: DBusClient.Client -> PP -- PP means pretty print
-- myPpHook dbus =
--   let colorize color string =
--         "%{F" <> color <> "} " <> string <> " %{F-}"
  
--       background = "#0B2422" -- dark green
--       secondary  = "#1E5954" -- green
--       accent     = "#51A59D" -- light green
--       text       = "#E9FFFA" -- white like green
      
--   in  def { ppOutput          = dbusOutput dbus
--           , ppCurrent         = colorize accent
--           , ppUrgent          = colorize text
--           , ppHidden          = colorize secondary
--           , ppTitle           = colorize accent . shorten 90
--           , ppSep             = "   "
--           }
myPpHook :: DBusClient.Client -> PP -- PP means pretty print
myPpHook dbus =
  def { ppOutput  = dbusOutput dbus . wrap "(box :space-evenly false :vexpand true" ")"
      , ppCurrent = wrapClass "ws-current"
      , ppUrgent  = wrapClass "ws-urgent"
      , ppHidden  = wrapClass "ws-hidden"
      , ppLayout  = wrapClass "ws-layout"
      , ppTitle   = \_ -> ""
      , ppSep     = "   "
      }
  where
    wrapClass c = wrap ("(label :class \"" <> c <> "\" :text \"") "\")"

myLogHookWithSignal dbus = dynamicLogWithPP (myPpHook dbus)

