{ mkDerivation, aeson, base, Diff, elm-export, fetchgit, hspec
, HUnit, lens, servant, servant-foreign, stdenv, text
, wl-pprint-text
}:
mkDerivation {
  pname = "servant-elm";
  version = "0.5.0.0";
  src = fetchgit {
    url = "https://github.com/mattjbray/servant-elm.git";
    sha256 = "1mfbd9bk5kkd6wdfhg42k0w8wccs1ffv8ndxvhsyffyswp3zwkbw";
    rev = "f665ddd5cefac26b3e56981f3516772d3987a6f3";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base elm-export lens servant servant-foreign text wl-pprint-text
  ];
  testHaskellDepends = [
    aeson base Diff elm-export hspec HUnit servant text
  ];
  doCheck = false;
  homepage = "http://github.com/mattjbray/servant-elm#readme";
  description = "Automatically derive Elm functions to query servant webservices";
  license = stdenv.lib.licenses.bsd3;
}
