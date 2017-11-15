{ compiler ? "ghc822" }:

let
  pkgs = import <nixpkgs> {};
  dontCheck = pkgs.haskell.lib.dontCheck;
  haskellPkgs = pkgs.haskell.packages."${compiler}".extend (self: super: {
    my3ml = self.callPackage ./my3ml.nix {};
    elm-export = self.callPackage ./elm-export.nix {};
    servant-elm = dontCheck super.servant-elm;
  });
in
  {
    my3ml = haskellPkgs.my3ml;
  }