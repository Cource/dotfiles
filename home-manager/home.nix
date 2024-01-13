{config, pkgs, ...}:

{
  home = {
    packages = with pkgs;
      [ (nerdfonts.override { fonts=["NerdFontsSymbolsOnly"]; })
        xmonad-log
        brightnessctl
        flameshot
        vesktop
        helvum
        logseq
        foliate
      ];
    stateVersion = "23.05";
  };
  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    prezto = {
      enable = true;
      caseSensitive = false;
      editor.keymap = "emacs";
      prompt.theme = "walters";
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
    extraPackages = epkgs: with epkgs; [
      leaf
	    leaf-keywords
      wildcharm-theme
      smooth-scrolling
      mood-line
      ligature
      vertico
      corfu
      embark
      orderless
      expand-region
      magit
      visual-fill-column
      # Language packages
      nix-mode
      elm-mode
      haskell-mode
      sass-mode
      haml-mode
      rust-mode
      yuck-mode
      sly
      org
    ];
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
          family = "Fira Code";
          style = "Regular";
        };
        size = 8;
      };
      window = {
        opacity = 0.9;
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

  services.easyeffects = {
    enable = true;
  };

  services.syncthing.enable = true;
}

