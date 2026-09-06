{ config, inputs, ... }:
{
  imports = [ ];

  perSystem =
    { pkgs, config, ... }:
    {
      legacyPackages = import ../package-set.nix { inherit pkgs; };

      packages = {
        inherit (config.legacyPackages) drvgraph;
      };
    };
}
