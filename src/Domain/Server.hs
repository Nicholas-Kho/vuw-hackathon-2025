module Domain.Server (runApp) where

import Api.Backend
import App
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Random.Strict
import qualified Data.List.NonEmpty as N
import qualified Data.Set as S
import Domain.Logic (drunkardsWalk, expandNode, lookupNodes, randomFromStore, verifyNodeId)
import Network.Wai.Handler.Warp (run)
import Servant
import TePapa.Env (getPort, getStaticPath, loadDotEnv)

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

runApp :: IO ()
runApp = do
    loadDotEnv
    port <- getPort
    appEnv <- getInitialEnv
    staticPath <- getStaticPath
    putStrLn $ "Listening on port " <> (show port)
    run port (app appEnv staticPath)

apiServer :: ServerT ApiRoutes RAppM
apiServer = serveStart :<|> serveExpand

serveExpand :: ExpandParams -> RAppM (Maybe Subgraph)
serveExpand ExpandParams{expandAboutId = unid} =
    lift (verifyNodeId . toInt $ unid) >>= \case
        Nothing -> pure Nothing
        Just nid -> do
            outIds <- lift $ expandNode nid
            results <- lift $ lookupNodes (S.toList outIds)
            pure . Just . Subgraph $ results

serveStart :: RAppM InitialGameState
serveStart = do
    (nid, _) <- randomFromStore lift
    path <- drunkardsWalk lift nid 10
    pure $
        InitialGameState
            { subgraph = Subgraph $ N.toList path
            , startAt = nid
            , endAt = fst . N.last $ path
            }

serveStatic :: FilePath -> ServerT Raw RAppM
serveStatic = serveDirectoryFileServer
