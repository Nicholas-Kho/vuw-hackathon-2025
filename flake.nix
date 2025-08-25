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
        # This should match the resolver in stack.yaml
        ghcVersion = "ghc9102";
        ghcPkgSet = pkgs.haskell.packages.${ghcVersion};
      in {
        # The devShell provides all system dependencies that stack.yaml expects,
        # including GHC, haskell-language-server, zlib, pkg-config, etc.
        # Stack is run with `system-ghc: true`, so it will use the GHC from here
        # instead of downloading its own.
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Frontend:
            nodejs_24
            # Backend:
            stack
            ## Haskell editor tools:
            fourmolu
            hlint
            ghcPkgSet.ghc
            ghcPkgSet.haskell-language-server
            ## Dependencies of some haskell packages:
            zlib
            pkg-config
            ## DB
            sqlite
          ];

          shellHook = ''
            # Replace the wrapper with a symlink to the working binary
            # for some reason the wrapper links to some language server build with a slightly different ghc..
            # I have no idea where it's coming from, because I've checked and there is only one ghc that gets
            # pulled in, which is the one from this shell. Thankfully the other binaries work. I don't know what
            # the wrapper is finding, but here we can force it.
            HLS_BIN=$(command -v haskell-language-server-9.10.2)
            if [ -n "$HLS_BIN" ]; then
              mkdir -p .dev-bin
              ln -sf "$HLS_BIN" .dev-bin/haskell-language-server-wrapper
              export PATH="$(pwd)/.dev-bin:$PATH"
              echo "haskell-language-server-wrapper → $HLS_BIN"
            else
              echo "Warning: could not find haskell-language-server-9.10.2"
            fi
          '';
        };
      });
}

