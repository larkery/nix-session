{
   packageOverrides = pkgs: rec {
     dmenu = pkgs.dmenu.override {
       patches = [ ../../patches/dmenu-number-output.patch ];
     };
     #emacs24 has a silly bug with xft fonts.
     emacs = pkgs.lib.overrideDerivation
       (pkgs.emacs.override {
         withGTK3 = false;
         withGTK2 = false;
         })
       (attrs : { patches = pkgs.emacs.patches ++ [ ../../patches/emacs-xft.patch ]; }) ;

     epass = import ../../epass;

     workspace = pkgs.buildEnv {
       name = "workspace";

       paths = with pkgs; [
       emacs
       arandr
       cbatticon
       dmenu
       dunst
       file
       git
       gnupg21
       gtk-engine-murrine
       i3blocks
       i3lock
       isync
       libnotify
       man-pages
       msmtp
       networkmanagerapplet
       notmuch
       nox
       numix-gtk-theme
       numix-icon-theme
       gnome.gnome_icon_theme
       openjdk
       openssl
       pa_applet
       pinentry
       silver-searcher
       smbnetfs
       strace
       vcprompt
       vimb
       w3m
       which
       xclip
       xorg.xkill
       xsettingsd
       xss-lock
       xorg.xbacklight
       xbrightness
       unzip
       jq
       epass
       ];

       postBuild = ''
       echo "generating info directory (glurp)"
       mkdir -p $out/indexes/iinfo
       for infofile in "$out/share/info/"*.info*; do
          ${pkgs.texinfo}/bin/install-info "$infofile" "$out/indexes/iinfo/dir"
       done
       '';
     };
   };
}
