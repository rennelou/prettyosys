{
  pkgs ? import <nixpkgs> {}
}:

pkgs.stdenv.mkDerivation rec {
  name = "example";
  buildInputs = [
    pkgs.haskell.compiler.ghc8107
    pkgs.git
    pkgs.zlib
    pkgs.cabal-install
    pkgs.pkgconfig
    pkgs.which
  ];
}
