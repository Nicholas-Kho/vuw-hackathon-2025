{
  description = "Development tools for MuseLinks";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:numtide/flake-utils";
    mkElmDerivation.url = "github:jeslie0/mkElmDerivation";
  };

  outputs = { nixpkgs, flake-utils, mkElmDerivation, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ mkElmDerivation.overlays.default ];
        };
        ghcVersion = "ghc9102";
        elmPkgs = pkgs.elmPackages;
        haskellPkgs = pkgs.haskell.packages.${ghcVersion}.override {
          overrides = self: super: {
            system-fileio = pkgs.haskell.lib.dontCheck super.system-fileio;
          };
        };
        muselinks-server = haskellPkgs.callCabal2nix "muselinks" ./. { };
        muselinks-api-codegen = haskellPkgs.callCabal2nix "api-codegen" ./. { };
        muselinks-frontend = pkgs.mkElmDerivation {
          name = "muselinks-frontend";
          src = ./frontend;
          elmJson = ./frontend/elm.json;
          nativeBuildInputs = [ elmPkgs.elm muselinks-api-codegen ];
          buildPhase = ''
            mkdir -p src/Generated
            # TODO: Later, we won't want to hardcode this.
            ${muselinks-api-codegen}/bin/api-codegen src/ -l 8080
            elm make Main.elm --output=index.html
          '';
          installPhase = ''
            mkdir -p $out/
            cp index.html $out/
          '';
        };
      in {
        packages.default = pkgs.symlinkJoin {
          name = "muselinks";
          paths = [ muselinks-server muselinks-frontend ];
          nativeBuildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            mkdir -p $out/static
            ln -s ${muselinks-frontend}/index.html $out/static/index.html
            ln -s ${muselinks-frontend}/assets $out/static/assets
            rm -f $out/bin/muselinks
            makeWrapper ${muselinks-server}/bin/muselinks $out/bin/muselinks --set STATIC_PATH $out/static
          '';
        };
        devShells.default = haskellPkgs.shellFor {
          packages = p: [ muselinks-server muselinks-api-codegen ];
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
            elmPkgs.elm-live
          ];
        };
      });
}

