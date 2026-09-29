{
  pkgs ? (import ../.).inputs.nixpkgs.legacyPackages.${builtins.currentSystem},
  lib ? pkgs.lib,
}:
let
  rootDir = ../.;

  haskellPkgs = pkgs.haskellPackages;
  haskellLib = pkgs.haskell.lib;

  selfPackageNames =
    let
      allEntries = builtins.readDir (rootDir + /hs-packages);
      dirTypeEntries = lib.attrsets.filterAttrs (_: type: type == "directory") allEntries;
      entryNames = builtins.attrNames dirTypeEntries;
    in
    entryNames;

  internalPkgs =
    let
      pair = name: value: { inherit name value; };

      makePackage =
        hpkgs: name:
        if builtins.pathExists (rootDir + /hs-packages/${name}/package.nix) then
          hpkgs.callPackage (rootDir + /hs-packages/${name}/package.nix) { }
        else
          let
            src = import (rootDir + /nix/lib/make-package-source.nix) { inherit lib name; };
            cabal2nixOpts = {
              extraCabal2nixOptions = "--subpath 'hs-packages/${name}'";
              srcModifier = lib.id;
            };
          in
          hpkgs.callCabal2nixWithOptions name src cabal2nixOpts { };

      extendedFullPkgs = haskellPkgs.override (attrs: {
        overrides = final: prev: lib.attrsets.genAttrs selfPackageNames (makePackage final);
      });

      exportedPkgs =
        let
          exportPkgs =
            fullPkgs:
            (lib.attrsets.genAttrs selfPackageNames (name: fullPkgs.${name}))
            // {
              override = arg: exportPkgs (fullPkgs.override arg);
            };
        in
        exportPkgs extendedFullPkgs;
    in
    exportedPkgs;
in
lib.fix (self: {
  inherit haskellPkgs internalPkgs;

  default = self.drvgraph;

  drvgraph = haskellLib.justStaticExecutables internalPkgs.drvgraph;
})
