{-# LANGUAGE OverloadedStrings #-}

module Domain.Server (runApp) where

import Api.Backend
import App
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Random.Strict
import qualified Data.List.NonEmpty as N
import qualified Data.Set as S
import Domain.Logic (drunkardsWalk, expandNode, lookupNodes, randomFromStore, verifyNodeId)
import Domain.Model (elmify)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, simpleCorsResourcePolicy)
import Servant
import TePapa.Env (getPort, getStaticPath, getUseCors, loadDotEnv)
import Text.Read (readMaybe)

type RAppM = RandT StdGen AppM

-- TODO: Make this more readable, and catch errors thrown in AppM so they
-- don't crash the server thread.
nt :: AppEnv -> RAppM a -> Handler a
nt env appAction = do
    gen <- newStdGen
    Handler . ExceptT $ Right <$> (evalRandT appAction gen) `runAppM` env

app :: AppEnv -> FilePath -> Application
app appEnv staticPath =
    serve (Proxy @BackendApi) $
        hoistServer (Proxy @BackendApi) (nt appEnv) (apiServer :<|> serveStatic staticPath)

devCors :: CorsResourcePolicy
devCors =
    simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type"]
        }

runApp :: IO ()
runApp = do
    loadDotEnv
    port <- getPort
    appEnv <- getInitialEnv
    staticPath <- getStaticPath
    useCors <- getUseCors
    let corsMiddleware = if useCors then cors (const $ Just devCors) else id
    putStrLn $ "Listening on port " <> (show port)
    run port $ corsMiddleware (app appEnv staticPath)

apiServer :: ServerT ApiRoutes RAppM
apiServer = serveStart :<|> serveExpand

serveExpand :: ExpandParams -> RAppM (Maybe Subgraph)
serveExpand ExpandParams{expandAboutId = unid} =
    case readMaybe @Int (toString unid) of
        Nothing -> pure Nothing
        Just k ->
            lift (verifyNodeId k) >>= \case
                Nothing -> pure Nothing
                Just nid -> do
                    outIds <- lift $ expandNode nid
                    results <- lift $ lookupNodes (S.toList outIds)
                    pure . Just . Subgraph $ (fmap (\(x, c) -> (x, elmify c)) results)

serveStart :: RAppM InitialGameState
serveStart = do
    (nid, _) <- randomFromStore lift
    path <- drunkardsWalk lift nid 10
    pure $
        InitialGameState
            { subgraph = Subgraph $ fmap (\(x, c) -> (x, elmify c)) (N.toList path)
            , startAt = nid
            , endAt = fst . N.last $ path
            }

serveStatic :: FilePath -> ServerT Raw RAppM
serveStatic = serveDirectoryFileServer
