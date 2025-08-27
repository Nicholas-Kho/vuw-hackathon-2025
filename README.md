# VUW Hackathon 2025 - MuseLinks

## **MAIN IS CURRENTLY NOT WORKING!**
We are in the middle of re-writing the back end and front end. As of now, the backend builds, but is just set to query an API endpoint. This notice will be removed when necessary.

## Backend
located in `source/backend`

## Frontend
located in `source/frontend/src`
`app.js` is the file that runs when you `npm run start`


## Dependencies:

### Haskell tools / stack for backend
**If you are using the Nix package manager:** Run `nix develop` to pull in Stack, as well as all dependencies needed to build the backend (GHC, Zlib, pkg-config, etc.) as well as the language server.

**If you are not using Nix:** You will need to install Stack, and you'll probably want the language server too. An easy way to get these is via GHCup. The formatter used is fourmolu - you can either get this via your system's package manager, or install Cabal through GHCup and `cabal install fourmolu`. (this will put it in `~/.cabal/bin`). You may also need to install Zlib, as it is a non-Haskell dependency, so Stack won't manage it for you if it's missing and you will just get a build error.

Then simply go to `source/backend` and run `stack build --file-watch`

### .env:
in `source/backend`, create a `.env` file containing the following contents:
```
API_KEY = <Your key here>
PORT = <Whatever port the backend should run on. Default to 8080>
```

### Database:
There is a minimal `seed.sqlite3`. You can make a copy named `db.sqlite3` to get a local development database. Please refrain from commiting database related files, as they can blow up diffs and cause a lot of unnecessary conflicts.

### frontend: nodejs
`install npm` (ask chatgpt)

### homepage - ignore for now
`npm install gsap`
`npm i three @react-three/fiber`
`npm install framer-motion`

## Deployment
- Two terminals are needed.
- in `source/backend`, run `stack run`. This loads the backend
- in `source/frontend`, run `npm run start`, loading the frontend
