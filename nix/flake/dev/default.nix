{ config, inputs, ... }:
{
  imports = [
    ./devshells.nix
  ];

  perSystem =
    { system, pkgs, ... }:
    {
      _module.args.pkgsDev = pkgs;
    };

  flake.inputsDev = inputs;
}
