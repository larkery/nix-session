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
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";
  boot.loader.timeout = 1;

  networking = {
     firewall.enable = false;
     hostName = "keats"; # Define your hostname.
     domain = "cse.org.uk";
     extraHosts = ''
     127.0.0.1 keats.cse.org.uk
     127.0.0.1 keats
     '';
     networkmanager.enable = true;
     networkmanager.packages = with pkgs; [networkmanager_pptp];
  };

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
     cifs_utils
  ];

  # List services that you want to enable:

  programs.ssh.startAgent = false;
  hardware.pulseaudio.enable = true;

  # zsh has an annoying default config which I don't want
  # so to make it work well I have to turn it off first.
  programs.zsh = {
    enable = true;
    shellInit = "";
    shellAliases = {};
    promptInit = "";
    loginShellInit = "";
    interactiveShellInit = "";
    enableCompletion = false;
  };

  services = {
    upower.enable = true;
    nscd.enable = false;
    acpid.enable = true;
    tlp.enable = true;
    udisks2.enable = true;
    xserver = {
       enable = true;
       layout = "gb";
       synaptics = {
         enable = true;
         vertEdgeScroll = false;
         twoFingerScroll = true;
       };
       xkbOptions = "ctrl:nocaps";
       #windowManager.i3.enable = true;
       windowManager.xmonad = {
           enable = true;
           enableContribAndExtras = true;
           extraPackages = p : [p.taffybar];
       };
       displayManager.lightdm.enable = true;
    };

    autofs = {
      enable = true;
      timeout = 300;
      debug = true;
      autoMaster =
      let
        share = pkgs.writeScript "auto-share" ''
        #!/bin/sh
        NH="$AUTOFS_HOST"
        NS="$1"
        H="$AUTOFS_HOME"
        CF="$H/.passwords/run/$NH-$NS.credentials"
        rm -f -- "$CF"
        mkfifo -- "$CF"
        chown -- "$AUTOFS_USER" "$CF"
        echo "write-credentials $NH $NS" | ${pkgs.socat}/bin/socat STDIN UNIX-CONNECT:"$H/.passwords/run/socket" > /dev/null
        echo '-fstype=cifs,credentials='"$CF"',uid=$UID   ://'"$NH"'/'"$NS"
        '';
        host  = pkgs.writeText "auto-host"  "*   -fstype=autofs,-DAUTOFS_HOST=& program:${share}" ;
        top   = "/net file:${host}" ;
      in top;

    };
  };

  powerManagement.enable = true;

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
  
  fonts = {
    fonts = with pkgs; [
      inconsolata
      ubuntu_font_family
      font-droid
      dejavu_fonts
      source-sans-pro
      source-serif-pro
      fira
      fira-code
      fira-mono
    ];

    fontconfig.includeUserConf = true;

    fontconfig.hinting = {
        autohint = false;
        enable = true;
        style = "slight";
      };

    fontconfig.defaultFonts = {
      monospace = [ "Inconsolata" "DejaVu Sans Mono" ];
      sansSerif = [ "Ubuntu" ];
    };
  };
    # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";
}
