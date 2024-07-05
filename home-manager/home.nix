{config, pkgs, ...}:

{
  home = {
    packages = with pkgs;
      [ (nerdfonts.override { fonts=["NerdFontsSymbolsOnly"]; })
        xmonad-log
        brightnessctl
        flameshot
        libnotify
        grim slurp
        armcord
        stremio
        helvum
        logseq
        foliate
        wineWowPackages.base
        joshuto trash-cli
        qbittorrent
        mpv
      ];
    stateVersion = "24.05";
  };

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    localVariables = {
      PROMPT="%F{15}%n%F{8}@%m%F{15}%B|%b %f";
      RPROMPT="%F{8}%~%B%F{15}|%f%b";
    };
    shellAliases = {
      nonet = "systemd-run --scope -p IPAddressDeny=any";
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    extraConfig = builtins.readFile ./hyprland.conf;
  };

  # xsession = {
  #   enable = true;
  #   scriptPath = ".hm-xsession";
  #   windowManager.xmonad = {
  #     enable = true;
  #     enableContribAndExtras = true;
  #     extraPackages = hp: [
  #       hp.dbus
  #       hp.monad-logger
  #       hp.xmonad-contrib
  #     ];
  #     config = ./xmonad.hs;
  #   };
  # };

  services.picom = {
    enable = true;
    vSync = true;
    backend = "glx";
    fade = true;
    fadeDelta = 2;
    settings = { 
      corner-radius = 10;
      inactive-opacity-override = true;
    };
  };

  programs.emacs = {
    package = pkgs.emacs29;
    enable = true;
    extraPackages = epkgs: [epkgs.leaf];
    extraConfig = builtins.readFile ./emacs.el;
  };

  # programs.eww = {
  #   enable = true;
  #   configDir = ./eww-config;
  # };

  programs.waybar = {
    enable = true;
    settings = import ./waybar/waybar-config.nix;
    style = builtins.readFile ./waybar/style.css;
  };
  
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    theme = ./rofi.rasi;
  };

  services.mako = {
    enable = true;
    width = 350;
    height = 110;
    anchor = "bottom-right";
    layer = "overlay";
    font = "Jetbrains Mono 11.2";
    defaultTimeout = 5000;
    backgroundColor = "#333333";
    textColor = "#aaaaaa";
    borderColor = "#222222";
    borderSize = 8;
    borderRadius = 15;
    padding = "13,20";
    margin = "10,-15,0,0";
    extraConfig = ''
      outer-margin=20,0
      [urgency=high]
      text-color=#ffffff
      default-timeout=0
    '';
  };

  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal = {
          family = "JetBrains Mono";
          style = "Regular";
        };
        size = 12;
      };
      window = {
        opacity = 0.93;
        padding = {
          x = 5;
          y = 5;
        };
      };
    };
  };
  
  programs.firefox = {
    enable = true;
    profiles.cource = {
      isDefault = true;
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        ublock-origin
        sidebery
      ];
      search = {
        default = "DuckDuckGo";
        order = [
          "DuckDuckGo"
          "Google"
        ];
      };
      userChrome = ''
        #TabsToolbar { visibility: collapse !important; }
        #sidebar-header { display: none !important; }
        '';
    };
  };

  programs.git = {
    enable = true;
    userEmail = "jeffjacobjoy@gmail.com";
    userName = "cource";
  };


  services.syncthing.enable = true;
}

