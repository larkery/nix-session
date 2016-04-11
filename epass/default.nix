with import <nixpkgs> {};

pkgs.stdenv.mkDerivation {
  name = "epass-1.0.0";
  src = ./src/.;

  buildInputs = with pkgs;
  [ dmenu xdotool emacs jq ];

  installPhase = ''
  mkdir -p $out/bin
  mkdir -p $out/share/emacs/site-lisp/
  substitute epass $out/bin/epass \
    --replace jq ${pkgs.jq}/bin/jq \
    --replace dmenu ${pkgs.dmenu}/bin/dmenu \
    --replace emacs ${pkgs.emacs}/bin/emacs \
    --replace xdotool ${pkgs.xdotool}/bin/xdotool \
    --replace epass.el $out/share/emacs/site-lisp/epass.el
  cp epass.el $out/share/emacs/site-lisp/
  chmod +x $out/bin/*
  '';
}
