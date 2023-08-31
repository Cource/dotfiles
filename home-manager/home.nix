{config, pkgs, ...}:

let
  colors = {
    BG1 = "#1E5954";
    BG2 = "#0B2422";
    FG1 = "#E9FFFA";
    FG2 = "#51A59D";
  };
in
{
  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {inherit pkgs;};
  };

  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  #   }))
  # ];
  
  home-manager.users.cource = {config, ...}: {
    home = {
      packages = with pkgs;
        [ font-awesome
          light
          webcord
          helvum
          # (emacsWithPackagesFromUsePackage {
          #   config = ./emacs.el;
	        #   extraEmacsPackages = epkgs: [ epkgs.leaf ];
          #   defaultInitFile = true;
          #   alwaysEnsure = true;
          # })
        ];
      stateVersion = "22.11";
    };
    programs.zsh = {
      enable = true;
      enableSyntaxHighlighting = true;
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
      fadeDelta = 3;
      inactiveOpacity = 1;
      settings = { 
        corner-radius = 10;
      };
    };

    programs.emacs = {
      enable = true;
      extraPackages = epkgs: with epkgs; [
        leaf
	      leaf-keywords
        gruvbox-theme
        smooth-scrolling
        mood-line
        ligature
        vertico
        expand-region
        # Language packages
        nix-mode
        elm-mode
        haskell-mode
        sass-mode
        haml-mode
      ];
      extraConfig = builtins.readFile ./emacs.el;
    };
    services.emacs = {
      enable = false;
      client.enable = false;
    };

    services.polybar = import ./polybar.nix pkgs colors;
    systemd.user.services.polybar = {
      Install.WantedBy = [ "graphical-session.target" ];
    };
    
    programs.rofi= {
      enable = true;
      font = "Atkinson Hyperlegible 14";
      theme = with colors;
        let
          inherit (config.lib.formats.rasi) mkLiteral;
          darkGreen   = mkLiteral BG2;
          green       = mkLiteral BG1;
          lightGreen  = mkLiteral FG2;
          white       = mkLiteral FG1;
          transparent = mkLiteral "#00000000";
        in {
          "*" = {
            background-color = transparent;
            text-color = white;
            placeholder-color = lightGreen;
            width = 612;
          };
          "inputbar, element selected" = {
            background-color = green;
          };
          prompt = {
            enabled = false;
          };
          inputbar = {
            children = map mkLiteral [ "entry" ];
            margin = mkLiteral "0em 0em 0.8em 0em";
            padding = mkLiteral "0.5em 0.8em";
            border-radius = 10;
          };
          entry = {
            "placeholder" = "Search";
          };
          listview = {
            dynamic = true;
            lines = 10;
            spacing = 0;
            border-radius = 10;
          };
          element = {
            padding = mkLiteral "0.3em 0.5em";
            background-color = darkGreen;
          };
        };
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
  };
}
