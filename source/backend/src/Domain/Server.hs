module Domain.Server (runApp) where

import Api.Backend
import App
import Control.Concurrent.STM (atomically)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Random.Strict
import Control.Monad.Reader (asks)
import Domain.Logic (pickRandomFromStore)
import Network.Wai.Handler.Warp (run)
import Servant
import TePapa.Convert (GraphAction (AddNode))
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

serveExpand :: ExpandParams -> RAppM [GraphAction]
serveExpand params = error "todo"

serveStart :: RAppM InitialGameState
serveStart = do
    store <- lift $ asks graph
    (startId, startContent) <- pickRandomFromStore (liftIO . atomically) store
    pure $
        InitialGameState
            { subgraph = [AddNode startId startContent]
            , endAt = startId
            , startAt = startId
            }
