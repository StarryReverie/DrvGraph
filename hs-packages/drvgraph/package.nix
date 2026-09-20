{ mkDerivation, ansi-terminal, async, base, bytestring, containers
, directory, dlist, file-embed, filepath, hedgehog, hpack
, http-client, http-client-tls, http-types, lib, megaparsec, mtl
, network-uri, optics, optparse-applicative, process
, safe-exceptions, stm, stm-containers, tasty, tasty-discover
, tasty-hedgehog, tasty-hunit, text, transformers, unliftio
}:
mkDerivation {
  pname = "drvgraph";
  version = "0.1.0.0";
  src = import ../../nix/lib/make-package-source.nix { inherit lib; name = "drvgraph"; };
  postUnpack = "sourceRoot+=/./hs-packages/drvgraph/; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal async base bytestring containers directory dlist
    filepath http-client http-client-tls http-types megaparsec mtl
    network-uri optics optparse-applicative process safe-exceptions stm
    stm-containers text transformers unliftio
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    ansi-terminal async base bytestring containers directory dlist
    filepath http-client http-client-tls http-types megaparsec mtl
    network-uri optics optparse-applicative process safe-exceptions stm
    stm-containers text transformers unliftio
  ];
  testHaskellDepends = [
    ansi-terminal async base bytestring containers directory dlist
    file-embed filepath hedgehog http-client http-client-tls http-types
    megaparsec mtl network-uri optics optparse-applicative process
    safe-exceptions stm stm-containers tasty tasty-hedgehog tasty-hunit
    text transformers unliftio
  ];
  testToolDepends = [ tasty-discover ];
  prePatch = "hpack";
  license = "unknown";
  mainProgram = "drvgraph";
}
