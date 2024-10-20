{config, pkgs, ...}:

{
  home = {
    packages = with pkgs;
      [ (nerdfonts.override { fonts=["NerdFontsSymbolsOnly"]; })
        direnv
        brightnessctl
        swaybg
        libnotify
        grim slurp
        vesktop
        stremio
        helvum
        foliate
        wineWowPackages.base
        bottles gamescope
        joshuto trash-cli
        qbittorrent
        mpv
        moonlight-qt
        zen
        libreoffice-fresh
        gimp
        emacs29-pgtk
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
    initExtra = "eval \"$(direnv hook zsh)\"";
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

  home.file.".emacs".source = ./emacs.el;

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
    ignores = [ "**~" "**#" ];
  };

  services.lorri.enable = true;
  programs.jujutsu.enable = true;
}
