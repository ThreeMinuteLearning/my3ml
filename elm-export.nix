{ mkDerivation, aeson, base, bytestring, containers, Diff
, directory, fetchgit, formatting, hspec, hspec-core, HUnit, mtl
, QuickCheck, quickcheck-instances, stdenv, text, time
, wl-pprint-text
}:
mkDerivation {
  pname = "elm-export";
  version = "0.6.0.1";
  src = fetchgit {
    url = "https://github.com/tekul/elm-export.git";
    sha256 = "0wb7skmf2p60gg13skvjcvd38c7069zy81b9d90xmlsa4qa454rq";
    rev = "eba32f4f881728fd98575bfa91d8bee60279641c";
  };
  libraryHaskellDepends = [
    aeson base bytestring containers directory formatting mtl text time
    wl-pprint-text
  ];
  testHaskellDepends = [
    base bytestring containers Diff hspec hspec-core HUnit QuickCheck
    quickcheck-instances text time
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/krisajenkins/elm-export";
  description = "A library to generate Elm types from Haskell source";
  license = "unknown";
}
