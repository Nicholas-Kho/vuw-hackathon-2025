# VUW Hackathon 2025 - MuseLinks

## Note
This project is still in a very early stage. The front end is currently in development. The server serves an API to query the game graph (See `Api.Backend` for the Servant spec.) but does not serve an `index.html` or any other static files.

## Building and development

This project uses Nix to define the development environment and build system. Install Nix for your platform, then run
```
nix develop
```

This will provide all of the required tools and dependencies for development. Once you are in the dev-shell, you can run
 - `cabal run` in `source/backend` to build and run the backend.
 - `TODO` in `source/frontend` to build and run the frontend.

### .env:
in `source/backend`, create a `.env` file containing the following:
```
API_KEY=<Your key here>
```
You may wish to also set the following variables:

`MAX_CONCURRENT_HTTP=`: An integer that is at least `1`. Defaults to `8`. Determines the maximum number of in-flight HTTP requests to the Te Papa Collections API. Low values may noticeably increase the fetch time of new nodes. High values risk triggering ratelimits.

`PORT=`: An integer that is at least `2048`. Defaults to `8080`. Determines the port the backend server runs on.

`SEED=`: A Te Papa Collections reference of the form `NAMESPACE/ID`, where `NAMESPACE` is one of: `object`, `agent`, `place` and `ID` is a positive integer. Defaults to `object/1227923`. Determines the first node in the graph. During startup, the server attempts to parse this reference, look up the object, and convert it to a node. If this fails, the server process exits. You can find different seed objects and their identifiers using the [Te Papa Collections API browser](https://data.tepapa.govt.nz/docs/apibrowser.html). The `Type` field of your chosen object corresponds to `NAMESPACE`, and the `Id` field corresponds to `ID`.
