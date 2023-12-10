{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    inputs.home-manager.nixosModules.home-manager

    ./hardware-configuration.nix
    ./nvidia.nix
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];

    config = {
      allowUnfree = true;
    };
  };
  home-manager = {
    extraSpecialArgs = { inherit inputs outputs; };
    users = {
      cource = import ../home-manager/home.nix;
    };
  };

  nix = {
    registry = (lib.mapAttrs (_: flake: {inherit flake;})) ((lib.filterAttrs (_: lib.isType "flake")) inputs);
    nixPath = ["/etc/nix/path"];
    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
    };
  };
  environment.etc =
    lib.mapAttrs'
    (name: value: {
     name = "nix/path/${name}";
     value.source = value.flake;
     })
  config.nix.registry;

  networking.hostName = "msi";
  networking.networkmanager.enable = true;
  time.timeZone = "Asia/Kolkata";
  i18n.defaultLocale = "en_US.UTF-8";
  fonts.packages = [
    pkgs.fira-code
    pkgs.atkinson-hyperlegible
    pkgs.noto-fonts
  ];
  services.printing.enable = true;
  programs.dconf.enable = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 10;
  boot.loader.efi.canTouchEfiVariables = true;

  services = {
    gnome.gnome-keyring.enable = true;
    xserver = {
      enable = true;
      layout = "us";
      desktopManager.session = [{
        name = "home-manager";
        start = ''
          ${pkgs.runtimeShell} $HOME/.hm-xsession &
          waitPID=$!
        '';
      }];
      libinput = {
        enable = true;
        touchpad.naturalScrolling = true;
      };
      xkbOptions = "caps:ctrl_modifier";
    };
    
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  services = {
    upower.enable = true;
    dbus.enable = true;
  };
  systemd.services.upower.enable = true;

  users.users.cource = {
    isNormalUser = true;
    extraGroups = [ "wheel" "adbusers" "video" ];
    shell = pkgs.zsh;
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
  ];
  
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "steam"
    "steam-original"
    "steam-run"
  ];
  
  programs = {
    zsh.enable = true;
    adb.enable = true;
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };
  };

  networking.firewall.allowedTCPPorts = [ 8000 8080 ];

  system.stateVersion = "23.05";
}
