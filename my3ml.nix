{ mkDerivation, aeson, array, base, bytestring, containers
, contravariant, contravariant-extras, cryptonite, directory
, elm-export, errors, exceptions, hasql, hasql-pool, http-api-data
, jose-jwt, memory, monad-logger, mtl, random, rollbar
, safe-exceptions, servant-elm, servant-foreign, servant-server
, stdenv, stm, text, time, transformers, uuid, vector, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "my3ml";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers contravariant contravariant-extras
    cryptonite errors hasql hasql-pool http-api-data jose-jwt memory
    monad-logger mtl rollbar safe-exceptions servant-server stm text
    time transformers uuid vector wai
  ];
  executableHaskellDepends = [
    aeson array base bytestring containers cryptonite directory
    elm-export exceptions hasql hasql-pool jose-jwt monad-logger mtl
    random rollbar servant-elm servant-foreign servant-server stm text
    uuid wai wai-extra warp
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/ThreeMinuteLearning/my3ml";
  description = "Haskell backend for 3ML";
  license = stdenv.lib.licenses.bsd3;
}
