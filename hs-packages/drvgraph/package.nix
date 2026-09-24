{
  mkDerivation,
  aeson,
  ansi-terminal,
  async,
  base,
  bytestring,
  containers,
  directory,
  dlist,
  file-embed,
  filepath,
  hedgehog,
  hpack,
  http-client,
  http-client-tls,
  http-types,
  lib,
  megaparsec,
  mtl,
  network-uri,
  optics,
  optparse-applicative,
  safe-exceptions,
  stm,
  stm-containers,
  tasty,
  tasty-discover,
  tasty-hedgehog,
  tasty-hunit,
  text,
  unliftio,
}:
mkDerivation {
  pname = "drvgraph";
  version = "0.1.0.0";
  src = import ../../nix/lib/make-package-source.nix {
    inherit lib;
    name = "drvgraph";
  };
  postUnpack = "sourceRoot+=/./hs-packages/drvgraph/; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    ansi-terminal
    async
    base
    bytestring
    containers
    directory
    dlist
    filepath
    http-client
    http-client-tls
    http-types
    megaparsec
    mtl
    network-uri
    optics
    optparse-applicative
    safe-exceptions
    stm
    stm-containers
    text
    unliftio
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson
    ansi-terminal
    async
    base
    bytestring
    containers
    directory
    dlist
    filepath
    http-client
    http-client-tls
    http-types
    megaparsec
    mtl
    network-uri
    optics
    optparse-applicative
    safe-exceptions
    stm
    stm-containers
    text
    unliftio
  ];
  testHaskellDepends = [
    aeson
    ansi-terminal
    async
    base
    bytestring
    containers
    directory
    dlist
    file-embed
    filepath
    hedgehog
    http-client
    http-client-tls
    http-types
    megaparsec
    mtl
    network-uri
    optics
    optparse-applicative
    safe-exceptions
    stm
    stm-containers
    tasty
    tasty-hedgehog
    tasty-hunit
    text
    unliftio
  ];
  testToolDepends = [ tasty-discover ];
  prePatch = "hpack";
  license = "unknown";
  mainProgram = "drvgraph";
}
