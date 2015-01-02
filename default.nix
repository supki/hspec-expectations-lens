{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: rec {
  pname = "hspec-expectations-lens";
  version = "0.4.0";
  src = ./.;
  buildDepends = with haskellPackages; [ hspec hspecExpectations HUnit lens ];
  testDepends = with haskellPackages; buildDepends ++ [ silently ];
  meta = {
    homepage = "http://supki.github.io/hspec-expectations-lens/";
    description = "Hspec expectations for the lens stuff";
    license = self.stdenv.lib.licenses.bsd3; # lies
    platforms = self.ghc.meta.platforms;
  };
})
