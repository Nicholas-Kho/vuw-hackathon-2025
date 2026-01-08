# VUW Hackathon 2025 - MuseLinks

## **MAIN IS CURRENTLY NOT WORKING!**
We are in the middle of re-writing the back end and front end. As of now, the backend builds, but is just set to query an API endpoint. This notice will be removed when necessary.

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
PORT=<Port the backend should run on. Default to 8080>
```

### Database:
There is a minimal `seed.sqlite3`. You can make a copy named `db.sqlite3` to get a local development database. Please do not commit database related files, as they can grow large, produce noisy diffs, and cause unnecessary merge conflicts. You may, however, commit the `seed.sqlite3` file if you change the database schema or something related.
