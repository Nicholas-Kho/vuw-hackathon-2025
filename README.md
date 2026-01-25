# VUW Hackathon 2025 - MuseLinks

## Note:
This project uses the Nix package manager to define the development environment and build system. You can find installation instructions [on their website](https://nixos.org/download/).

## Quick start
1. [Register for](https://data.tepapa.govt.nz/docs/register.html) (or otherwise obtain) a Te Papa Collections API key.

2. Create a `.env` file with these contents:
```
API_KEY=<your API key here>
```
Optionally, see [configuration](#configuration) for a list of other variables and what they do.

3. Run
```
nix run github.com:Nicholas-Kho/vuw-hackathon-2025
```


## Building and development
The build process is:

1. Build the backend and code generator with Cabal
2. Invoke the code generator to make Elm bindings to the backend API
3. Build the frontend with the Elm compiler and move it to `static/`

`nix build` will do all of this for you. However, using `nix build` for development is awkward as the full build process may take several seconds. As an alternative, run
```
nix develop
```
This will provide all of the required tools and dependencies for development. In the dev shell, you may use:
 - `ghciwatch` for live compiler feedback on the Haskell part of the project
 - `elm reactor` for live compiler feedback on the Elm part of the project as well as a development server. You should run this in `frontend/`. 
 - `cabal run api-codegen <path to frontend/src>` to generate the Elm API bindings. You must do this if you change the backend API spec or any types deriving Elm definitions.
 - `cabal run muselinks` to run the main app

If it's your first time in the dev-shell, you should run
```
cabal run api-codegen frontend/src
```
before developing or running the frontend to ensure the backend bindings are there. Elm should complain pretty loud if you forget to do this, or if the bindings are outdated.

## Configuration:
in the root of the repository, create a `.env` file containing the following:
```
API_KEY=<Your key here>
```
You may wish to also set the following variables:

`MAX_CONCURRENT_HTTP=`: An integer that is at least `1`. Defaults to `8`. Determines the maximum number of in-flight HTTP requests to the Te Papa Collections API. Low values may noticeably increase the fetch time of new nodes. High values risk triggering ratelimits.

`PORT=`: An integer that is at least `2048`. Defaults to `8080`. Determines the port the backend server runs on.

`SEED=`: A Te Papa Collections reference of the form `NAMESPACE/ID`, where `NAMESPACE` is one of: `object`, `agent`, `place` and `ID` is a positive integer. Defaults to `object/1227923`. Determines the first node in the graph. During startup, the server attempts to parse this reference, look up the object, and convert it to a node. If this fails, the server process exits. You can find different seed objects and their identifiers using the [Te Papa Collections API browser](https://data.tepapa.govt.nz/docs/apibrowser.html). The `Type` field of your chosen object corresponds to `NAMESPACE`, and the `Id` field corresponds to `ID`.

`STATIC_PATH`: The path to the directory where the server will serve static files from. Defaults to `static/`. If you are running the server via `nix run`, this variable will already be set.
