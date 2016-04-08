{
   packageOverrides = pkgs: rec {
     dmenu = pkgs.dmenu.override {
       patches = [ ../../patches/dmenu-number-output.patch ];
     };
     #emacs24 has a silly bug with xft fonts.
     emacs = pkgs.lib.overrideDerivation pkgs.emacs (attrs : {
       patches = pkgs.emacs.patches ++ [ ../../patches/emacs-xft.patch ];
     });

     workspace = pkgs.buildEnv {
       name = "workspace";

       paths = with pkgs; [
         emacs
         strace
         msmtp
       ];

       postBuild = ''
       echo "generating info directory (glurp)"
       mkdir -p $out/indexes/iinfo
       for infofile in "$out/share/info/"*.info*; do
          ${pkgs.texinfo}/bin/install-info "$infofile" "$out/indexes/iinfo/dir"
       done
       echo "generating apropos index"
       # makewhatis does not work, but to be honest maybe something else would be easier
       # perhaps just do some kind of sqlite thing
       '';
     };
   };
}
