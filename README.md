# DrvGraph

## Overview

Traverse and analyze the dependency graph of a Nix package, and show what needs to be built or fetched in a tree representation.

## Usage

There are 4 ways to use it:

```sh
# Using Nix 3 and flakes
drvgraph -3 github:StarryReverie/DrvGraph#packages.x86_64-linux.drvgraph
# Using Nix 2
drvgraph -2 . -A legacyPackages.x86_64-linux.drvgraph
# Use the derivation from the positional argument
drvgraph $(nix-instantiate . -A legacyPackages.x86_64-linux.drvgraph)
# Use the derivation from the pipe
nix-instantiate . -A legacyPackages.x86_64-linux.drvgraph | drvgraph 
```

An example output consisting of `Existed`, `Unbuilt`, `Unsynced` and `Visited` labels (`--show-existed` and `--show-visited` are used in this example):

```sh
$ drvgraph -3 github:StarryReverie/DrvGraph#packages.x86_64-linux.drvgraph --show-existed --show-visited
Unbuilt drvgraph-0.1.0.0 v9lkj73m drv:bvahy93k
├─ Existed megaparsec-9.7.0 090yz9cq
├─ Existed stm-containers-1.2.2 0hdza9fr
├─ Existed tasty-1.5.4 0kk3dccy
├─ Existed network-uri-2.6.4.2 3g89qbjx
├─ Existed tasty-hedgehog-1.4.0.2 5mfcrqx5
├─ Existed http-client-tls-0.3.6.4 6pvdkk97
├─ Existed dlist-1.0 6xlj8d8k
├─ Existed glibc-locales-2.42-84 7ymncw12
├─ Existed optparse-applicative-0.18.1.0 bg1jm1yz
├─ Existed ghc-9.10.3 c5j2gll8
├─ Existed async-2.2.6 cmw3zr16
├─ Existed stdenv-linux dp0zdsys
├─ Existed tasty-discover-5.0.2 gzj7xmaf
├─ Existed ansi-terminal-1.1.5 h503l4qj
├─ Unsynced remove-references-to hn6ncvlw
│  └─ Existed bash-5.3p15 svx59425
├─ Existed hedgehog-1.5 j6jb2ccj
├─ Existed optics-0.4.2.1 j9nm689r
├─ Existed file-embed-0.0.16.0 kirz6djy
├─ Existed unliftio-0.2.25.1 p6kxzyh6
├─ Existed http-client-0.7.19 pl5aim7z
├─ Existed hpack-0.38.3 s0ggvhaa
├─ Visited bash-5.3p15 svx59425
├─ Unsynced haskell-generic-builder-test-wrapper.sh vqpza2mw
│  └─ Visited bash-5.3p15 svx59425
├─ Existed tasty-hunit-0.10.2 w6zv4vx8
├─ Existed safe-exceptions-0.1.7.4 wl01awpc
├─ Existed http-types-0.12.4 x1a6i5mv
└─ Existed coreutils-9.11 xjl7p8dv
```

Note that the result may differ, since some store paths may or may not exist.

## Cache

This project has a dedicated [Cachix substituter](https://app.cachix.org/organization/drvgraph/cache/drvgraph). You can optionally add the substituter URL <https://drvgraph.cachix.org/> to your configurations, either the vanilla `nix.settings.substituters` or `drvgraph`'s configuration file itself. Don't forget to add the public key `drvgraph.cachix.org-1:OngTuA0ekssxvRfZnRAvq1shmPRfJu3EO135RcbUvBg=`.

Note that `drvgraph.cachix.org` depends on `nix-community.cachix.org` to avoid caching duplicated store paths. It's recommended to also add `nix-community.cachix.org` to your substituter lists to prevent unexpected cache miss.

## License

This project is licensed under [GPL-3.0-or-later](./LICENSE) for all Haskell source codes (`./hs-packages`), [MIT](./LICENSE-NIX) for Nix source code (`flake.nix`, `nix/`), and [CC-BY-SA-4.0](./LICENSE-DOCS) for documentation (`docs/`).
