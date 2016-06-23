{
   packageOverrides = super: let self=super.pkgs; in rec {
     dmenu = super.dmenu.override {
       patches = [ ../../patches/dmenu-number-output.patch ];
     };
     #emacs24 has a silly bug with xft fonts.
     emacs = super.lib.overrideDerivation
       (super.emacs.override {
         withGTK3 = false;
         withGTK2 = false;
         })
       (attrs : { patches = super.emacs.patches ++ [ ../../patches/emacs-xft.patch ]; }) ;

     epass = import ../../epass;

     workspace = super.buildEnv {
       name = "workspace";

       paths = with super; [
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
       gnome.gnome_icon_theme
       arc-gtk-theme
       numix-icon-theme
       openjdk
       openssl
       pa_applet
       pinentry_qt4
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
          ${super.texinfo}/bin/install-info "$infofile" "$out/indexes/iinfo/dir"
       done
       '';
     };
   };
}
