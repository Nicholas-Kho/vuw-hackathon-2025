{
  description = "Development tools for MuseLinks";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghcVersion = "ghc9102";
        haskellPkgs = pkgs.haskell.packages.${ghcVersion}.override {
          overrides = self: super: {
            system-fileio = pkgs.haskell.lib.dontCheck super.system-fileio;
          };
        };
        backend = haskellPkgs.callCabal2nix "backend" ./source/backend { };
      in {
        packages.default = backend;
        devShells.default = haskellPkgs.shellFor {
          packages = p: [ backend ];
          nativeBuildInputs = with pkgs; [
            haskellPkgs.haskell-language-server
            fourmolu
            hlint
            sqlite
            cabal-install
          ];
        };
      });
}

