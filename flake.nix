{
  description = "Tools for working on thing";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ python313Packages.pip nodejs_24 ];
          shellHook = ''
            echo "Entering Python development shell..."
            # Create and activate venv if it doesn't exist
            if [ ! -d ".venv" ]; then
              python3 -m venv .venv
            fi
            source .venv/bin/activate
          '';
        };
      });
}

