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
        elmPkgs = pkgs.elmPackages;
        haskellPkgs = pkgs.haskell.packages.${ghcVersion}.override {
          overrides = self: super: {
            system-fileio = pkgs.haskell.lib.dontCheck super.system-fileio;
          };
        };
        muselinks = haskellPkgs.callCabal2nix "muselinks" ./. { };
        # muselinks-api-codegen = haskellPkgs.callCabal2nix "api-codegen" ./. { };
      in {
        packages.default = muselinks;
        devShells.default = haskellPkgs.shellFor {
          packages = p: [ muselinks ];
          nativeBuildInputs = with pkgs; [
            haskellPkgs.haskell-language-server
            haskellPkgs.fourmolu
            haskellPkgs.stan
            cabal-install
            ghciwatch
            elmPkgs.elm
            elmPkgs.elm-analyse
            elmPkgs.elm-format
            elmPkgs.elm-language-server
          ];
        };
      });
}

