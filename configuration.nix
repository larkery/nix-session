# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.timeout = 1;
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "hesse"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  i18n = {
     consoleFont = "Lat2-Terminus16";
     consoleUseXkbConfig = true;
     defaultLocale = "en_GB.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
     zsh
  ];

  # List services that you want to enable:

  programs.ssh.startAgent = false;
  programs.zsh.enable = true;

  hardware.pulseaudio.enable = true;

  services.acpid.enable = true;
  powerManagement.enable = true;
  services.tlp.enable = true;

# these do not appear to work :/
  services.udisks2.enable = true;

  networking.networkmanager.enable = true;
  networking.networkmanager.packages = with pkgs; [networkmanager_pptp];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "gb";

  services.xserver.synaptics = {
    enable = true;
    vertEdgeScroll = false;
    twoFingerScroll = true;
  };

  services.xserver.xkbOptions = "ctrl:nocaps";
  services.xserver.windowManager.i3.enable = true;
  services.xserver.displayManager.lightdm.enable = true;

  # may make manpath work?
  environment.profileRelativeEnvVars = {
    MANPATH = [ "/man" "/share/man" ] ;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";
  users.extraUsers.hinton = {
    extraGroups = [ "wheel" "networkmanager" ];
    isNormalUser = true;
    uid = 1000;
  };

  fonts.fontconfig.hinting.style = "slight";

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";
}
