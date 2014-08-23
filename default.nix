# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, aeson, attoparsec, baseUnicodeSymbols, dataDefault
, groundhog, groundhogPostgresql, hssqlppp, httpTypes, lens, mtl
, postgresqlSimple, resourcePool, squealer, text, transformers
, unorderedContainers, yaml, yesodCore, yesodRoutes
}:

cabal.mkDerivation (self: {
  pname = "yesod-squealer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec baseUnicodeSymbols dataDefault groundhog
    groundhogPostgresql hssqlppp httpTypes lens mtl postgresqlSimple
    resourcePool squealer text transformers unorderedContainers yaml
    yesodCore yesodRoutes
  ];
  meta = {
    description = "Yesod REST API for Squealer databases";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})