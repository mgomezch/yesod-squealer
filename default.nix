# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, aeson, attoparsec, baseUnicodeSymbols, dataDefault
, groundhog, groundhogPostgresql, hssqlppp, httpTypes, lens
, monadLogger, mtl, networkUri, postgresqlSimple, resourcePool
, squealer, text, transformers, unorderedContainers, yaml
, yesodCore
}:

cabal.mkDerivation (self: {
  pname = "yesod-squealer";
  version = "0.1.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec baseUnicodeSymbols dataDefault groundhog
    groundhogPostgresql hssqlppp httpTypes lens monadLogger mtl
    networkUri postgresqlSimple resourcePool squealer text transformers
    unorderedContainers yaml yesodCore
  ];
  meta = {
    description = "Yesod REST API for Squealer databases";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
