{config, pkgs, ...}:

{
  home = {
    packages = with pkgs;
      [ (nerdfonts.override { fonts=["NerdFontsSymbolsOnly"]; })
        xmonad-log
        brightnessctl
        flameshot
        vesktop
        stremio
        helvum
        logseq
        foliate
        wineWowPackages.base
        joshuto
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
  
  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [
        hp.dbus
        hp.monad-logger
        hp.xmonad-contrib
      ];
      config = ./xmonad.hs;
    };
  };

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

  programs.eww = {
    enable = true;
    configDir = ./eww-config;
  };
  
  programs.rofi= {
    enable = true;
    theme = ./rofi.rasi;
  };

  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal = {
          family = "JetBrains Mono";
          style = "Regular";
        };
        size = 8;
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

