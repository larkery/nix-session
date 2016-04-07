{
   packageOverrides = pkgs: rec {
     dmenu = pkgs.dmenu.override {
       patches = [ ../../patches/dmenu-number-output.patch ];
     };
   };
}
