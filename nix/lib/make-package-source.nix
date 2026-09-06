{ lib, name }:
let
  rootDir = ../../.;
in
lib.fileset.toSource {
  root = rootDir;

  fileset = lib.fileset.unions [
    (rootDir + /package.common.yaml)
    (rootDir + /hs-packages/${name}/package.yaml)

    (lib.fileset.maybeMissing (rootDir + /hs-packages/${name}/app))
    (lib.fileset.maybeMissing (rootDir + /hs-packages/${name}/src))
    (lib.fileset.maybeMissing (rootDir + /hs-packages/${name}/test))

    (rootDir + /nix/lib/make-package-source.nix)
  ];
}

