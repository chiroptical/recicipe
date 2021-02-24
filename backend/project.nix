{ mkDerivation, base, countable-inflections, hpack, hspec
, hspec-megaparsec, megaparsec, parser-combinators, servant
, servant-server, stdenv, text, units
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base countable-inflections megaparsec parser-combinators servant
    servant-server text units
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base countable-inflections megaparsec parser-combinators servant
    servant-server text units
  ];
  testHaskellDepends = [
    base countable-inflections hspec hspec-megaparsec megaparsec
    parser-combinators servant servant-server text units
  ];
  prePatch = "hpack";
  homepage = "https://github.com/chiroptical/recicipe#readme";
  license = stdenv.lib.licenses.bsd3;
}
