{ compiler ? "default" }:

let
  pkgs = import <nixpkgs> {};
  dontCheck = pkgs.haskell.lib.dontCheck;
  hPkgs = if compiler == "default"
              then pkgs.haskellPackages
              else pkgs.haskell.packages.${compiler};

  haskellPkgs = hPkgs.extend (self: super: {
    my3ml = self.callPackage ./my3ml.nix {};
    elm-export = self.callPackage ./elm-export.nix {};
    servant-elm = self.callPackage ./servant-elm.nix {};
    text-builder = dontCheck super.text-builder;
    hasql-pool = dontCheck super.hasql-pool;
  });
in
  {
    my3ml = haskellPkgs.my3ml;
  }
