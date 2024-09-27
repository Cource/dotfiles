{ config, pkgs, lib, ... }:

{
  systemd.user.services.deadd = {
    description = "deadd notification center";
    serviceConfig.ExecStart = "${pkgs.deadd-notification-center}/bin/deadd-notification-center";
    wantedBy = [ "graphical-session.target" ]; # starts after login
  };
}
