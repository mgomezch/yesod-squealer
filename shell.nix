let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.myHaskellPackages.override {
    extension = self: super: {
      yesodSquealer = pkgs.myHaskellPackages.callPackage ./. {};
    };
  };

in
  pkgs.lib.overrideDerivation haskellPackages.yesodSquealer (attrs: {
    buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 ] ++ attrs.buildInputs;
  })
