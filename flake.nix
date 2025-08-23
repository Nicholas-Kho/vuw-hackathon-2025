{
  description = "Development tools for MuseLinks";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Frontend:
            nodejs_24
            # Backend:
            stack
            ## Haskell editor tools:
            haskell-language-server
            ormolu
            hlint
            ## Dependencies of some haskell packages:
            zlib
            pkg-config
            ## DB
            sqlite
          ];
        };
      });
}

