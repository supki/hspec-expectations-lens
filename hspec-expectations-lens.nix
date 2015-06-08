{ mkDerivation, base, hspec, hspec-expectations, HUnit, lens
, silently, stdenv
}:
mkDerivation {
  pname = "hspec-expectations-lens";
  version = "0.4.0";
  src = ./.;
  buildDepends = [ base hspec hspec-expectations HUnit lens ];
  testDepends = [ base hspec lens silently ];
  homepage = "http://supki.github.io/hspec-expectations-lens/";
  description = "Hspec expectations for the lens stuff";
  license = stdenv.lib.licenses.bsd2;
}
