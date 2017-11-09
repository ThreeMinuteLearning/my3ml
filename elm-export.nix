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
    sha256 = "12r3mjfy4912vbn75zv9n6rj4kq08hhn78ik2v7jxjfi4i2v3fh6";
    rev = "91326dce9fb4e7a087e3ca7bf97c4f2022585e60";
  };
  libraryHaskellDepends = [
    aeson base bytestring containers directory formatting mtl text time
    wl-pprint-text
  ];
  testHaskellDepends = [
    base bytestring containers Diff hspec hspec-core HUnit QuickCheck
    quickcheck-instances text time
  ];
  homepage = "http://github.com/krisajenkins/elm-export";
  description = "A library to generate Elm types from Haskell source";
  license = "unknown";
}
