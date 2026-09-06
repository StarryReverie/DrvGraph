{ config, inputs, ... }:
{
  perSystem =
    {
      pkgsDev,
      config,
      lib,
      ...
    }:
    {
      devShells.default = pkgsDev.haskellPackages.shellFor {
        packages =
          hpkgs:
          lib.pipe config.legacyPackages.internalPkgs [
            (lib.attrsets.filterAttrs (name: value: lib.isDerivation value))
            lib.attrsets.attrValues
          ];

        nativeBuildInputs = [
          pkgsDev.cabal-install
          pkgsDev.cabal2nix
          pkgsDev.hpack

          config.legacyPackages.haskellPkgs.haskell-language-server
          config.legacyPackages.haskellPkgs.hlint
          config.legacyPackages.haskellPkgs.implicit-hie
          pkgsDev.fourmolu
          pkgsDev.ghcid

          pkgsDev.nixfmt
          pkgsDev.treefmt

          pkgsDev.just
        ];

        withHoogle = true;

        shellHook = ''
          # Due to <https://github.com/haskell/cabal/issues/8434>, build tools provided by
          # Nix can't be recognized by cabal-install, while in Nix's own build process, the build
          # tools are invoked directly with `Setup.hs`, and in non-Nix development environments,
          # they are installed by cabal-install. Based on the workaround provided by
          # <https://github.com/NixOS/nixpkgs/issues/130556#issuecomment-2762237786>, we set the
          # `in-nix-shell` flag only inside a Nix shell, so that cabal won't try to build these
          # tools again here. To make things easier, this flag is written to `cabal.project.local`
          # automatically, if it doesn't exist before.
          project_root="$PWD"
          while [ ! -f "$project_root/cabal.project" ] && [ "$project_root" != "/" ]; do
            project_root="$(dirname "$project_root")"
          done
          if [ -f "$project_root/cabal.project" ] && [ ! -f "$project_root/cabal.project.local" ]; then
            echo 'flags: +in-nix-shell' > "$project_root/cabal.project.local"
          fi
        '';
      };
    };
}
