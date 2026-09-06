hpack:
  #!/usr/bin/env bash
  for entry in $(ls ./hs-packages/); do
    echo "::: Running hpack for package \"$entry\""
    hpack ./hs-packages/$entry/package.yaml
  done

cabal2nix:
  #!/usr/bin/env bash
  for entry in $(ls ./hs-packages/); do
    echo "::: Running cabal2nix for package \"$entry\""
    cabal2nix . \
      --hpack \
      --subpath ./hs-packages/$entry/ \
      --src-expression "import ../../nix/lib/make-package-source.nix { inherit lib; name = \"$entry\"; }" \
      > ./hs-packages/$entry/package.nix
  done

hoogle:
  @hoogle server --local --port 8080
