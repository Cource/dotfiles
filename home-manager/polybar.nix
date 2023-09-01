pkgs: colors: {
  enable = true;
  script = "polybar top &";
  settings = {
    "global/wm" = {
      margin-bottom = 0;
    };
    "bar/top" = {
      width = "100%";
      height = "3.5%";
      radius = 10;
      padding = 4;
      module-margin = 2;
      border = {
        left.size = 8;
        right.size = 8;
        top.size = 8;
        bottom.size = 0;
      };
      wm-restack = "generic";
      background = colors.BG2;
      foreground = colors.FG1;
      tray-position = "right";
      font-0 = "Atkinson Hyperlegible:style=Regular;2";
      font-1 = "Font Awesome 6 Free Solid:style=Solid;2";
      modules = {
        left = "xmonad";
        center = "date";
        right = "wired-network wireless-network wireplumber backlight battery";
      };
    };
    "module/date" = {
      type = "internal/date";
      internal = 5;
      date = "%d %b, %Y";
      time = "%H:%M";
      label = "%time%  %date%";
    };
    "module/xmonad" = {
      type = "custom/script";
      exec = "${pkgs.xmonad-log}/bin/xmonad-log";
      tail = true;
    };
    "module/network" = {
      type = "internal/network";
      accumulate-stats = true;
      unknown-as-up = true;
      format-connected = " <label-connected>";
      label-connected = "%essid%   %downspeed%  %upspeed%";
    };
    "module/wired-network" = {
      "inherit" = "module/network";
      interface-type = "wired";
      label-connected = " %ifname%  %downspeed:8%  %upspeed:8%";
      format-connected = "<label-connected>";
    };
    "module/wireless-network" = {
      "inherit" = "module/network";
      interface-type = "wireless";
    };
    "module/wireplumber" = {
      type = "custom/script";
      exec = "${pkgs.wireplumber}/bin/wpctl get-volume @DEFAULT_AUDIO_SINK@ | ${pkgs.gnused}/bin/sed 's/Volume: //;s/0.00/0/;s/0\\.//;s/\\.//;s/$/%/;s/.* //;s/]%/]/'";
      interval = 1;
      format = " <label>";
    };
    "module/backlight" = {
      type = "internal/backlight";
      card = "intel_backlight";
      enable-scroll = true;
      format = " <label>";
      label = "%percentage%%";
    };
    "module/battery" = {
      type = "internal/battery";
      battery = "BAT1";
      adapter = "ADP1";
      full-at = 98;
      format = {
        charging = "<animation-charging> <label-charging>";
        discharging = "<ramp-capacity> <label-discharging>";
        full = "<ramp-capacity> <label-full>";
        low = "<animation-low><ramp-capacity> <label-low>";
      };
      label = {
        charging = "%percentage%%";
        discharging = "%percentage%%";
        full = "Fully Charged";
        low = "Connect Charger! %percentage%%";
      };
      ramp = {
        capacity-4 = "";
        capacity-3 = "";
        capacity-2 = "";
        capacity-1 = "";
        capacity-0 = "";
      };
      animation = {
        charging-4 = "";
        charging-3 = "";
        charging-2 = "";
        charging-1 = "";
        charging-0 = "";
        charging-framerate = 750;
        low-0 = "!";
        low-1 = " ";
        low-framerate = 200;
      };
    };
  };
}
