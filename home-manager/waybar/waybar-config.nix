{
  mainBar = {
    "height"= 900;
      "width"= 40;
      "position"= "left";
      "layer"= "top";
      "output"= [
        "eDP-1"
        "HDMI-A-1"
      ];
      "hyprland/workspaces"= {
        "disable-scroll"= true;
      };
      "hyprland/window"= {
        "rotate"= 270;
      };
      "modules-left"= [
        "hyprland/workspaces"
        "wlr/taskbar"
        "hyprland/window"
      ];
      "modules-right"= [
        "tray"
        "network"
        "wireplumber"
        "backlight"
        "clock"
        "battery"
        "temperature"
      ];
      "clock"= {
        "format"= "{:%H\n%M}";
        "tooltip-format"= "<tt><small>{calendar}</small></tt>";
        "calendar"= {
          "mode"= "month";
          "mode-mon-col"= 3;
          "weeks-pos"= "right";
          "on-scroll"= 1;
          "on-click-right"= "mode";
          "format"= {
            "today"= "<span color='#a6e3a1'><b><u>{}</u></b></span>";
          };
        };
      };
      tray = {
        icon-size = "30";
      };
      "wireplumber"= {
        "format"= "{icon}";
        "format-bluetooth"= "{icon}";
        "tooltip-format"= "{volume}% {icon}";
        "format-muted"= "󰖁";
        "format-icons"= {
          "headphones"= "󰋌";
          "handsfree"= "󰋌";
          "headset"= "󰋌";
          "phone"= "";
          "portable"= "";
          "car"= " ";
          "default"= [
            "󰕿"
            "󰖀"
            "󰕾"
          ];
        };
        "on-click"= "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
        "on-scroll-up"= "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05+";
        "on-scroll-down"= "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05-";
        "smooth-scrolling-threshold"= 1;
      };
      "battery"= {
        "rotate"= 270;
        "states"= {
          "good"= 95;
          "warning"= 30;
          "critical"= 15;
        };
        "format"= "{icon}";
        "format-charging"= "<b>{icon}</b>";
        "format-full"= "<span color='#82A55F'><b>{icon}</b></span>";
        "format-icons"= [
          "󰁻"
          "󰁼"
          "󰁾"
          "󰂀"
          "󰂂"
          "󰁹"
        ];
        "tooltip-format"= "{timeTo} {capacity} % | {power} W";
      };
      "network"= {
        "format"= "{icon}";
        "format-icons"= {
          "wifi"= [
            "󰤨"
          ];
          "ethernet"= [
            "󰈀"
          ];
          "disconnected"= [
            "󰖪"
          ];
        };
        "format-wifi"= "󰤨";
        "format-ethernet"= "󰈀";
        "format-disconnected"= "󰖪";
        "format-linked"= "󰈁";
        "tooltip-format"= "{essid} | {bandwidthTotalBytes} | {ipaddr} | {ifname}";
        "on-click"= "pgrep -x rofi &>/dev/null && notify-send rofi || networkmanager_dmenu";
      };
      "backlight"= {
        "device"= "intel_backlight";
        "format"= "{icon}";
        "format-icons"= [
          ""
          ""
          ""
          ""
          ""
          ""
          ""
          ""
          ""
          ""
          ""
          ""
          ""
          ""
          ""
        ];
        "on-scroll-down"= "brightnessctl s 5%-";
        "on-scroll-up"= "brightnessctl s +5%";
        "tooltip"= true;
        "tooltip-format"= "Brightness= {percent}% ";
        "smooth-scrolling-threshold"= 1;
      };
  };
}
