module Domain.Server (runApp) where

import Api.Backend
import App
import Cache.NodeId (NodeId)
import Control.Concurrent.STM (atomically)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Random.Strict
import Control.Monad.Reader (asks)
import Domain.Logic (randomFromStore)
import Domain.Model (Node)
import Network.Wai.Handler.Warp (run)
import Servant
import TePapa.Env (getPort, loadDotEnv)

type RAppM = RandT StdGen AppM

-- TODO: Make this more readable, and catch errors thrown in AppM so they
-- don't crash the server thread.
nt :: AppEnv -> RAppM a -> Handler a
nt env appAction = do
    gen <- newStdGen
    Handler . ExceptT $ Right <$> (evalRandT appAction gen) `runAppM` env

app :: AppEnv -> Application
app appEnv =
    serve (Proxy @BackendApi) $
        hoistServer (Proxy @BackendApi) (nt appEnv) server

runApp :: IO ()
runApp = do
    loadDotEnv
    port <- getPort
    appEnv <- getInitialEnv
    putStrLn $ "Listening on port " <> (show port)
    run port (app appEnv)

server :: ServerT BackendApi RAppM
server = serveStart :<|> serveExpand

serveExpand :: ExpandParams -> RAppM [(NodeId, Node)]
serveExpand params = error "todo"

serveStart :: RAppM InitialGameState
serveStart = do
    (nid, node) <- randomFromStore lift
    pure $
        InitialGameState
            { subgraph = [(nid, node)]
            , startAt = nid
            , endAt = nid
            }
