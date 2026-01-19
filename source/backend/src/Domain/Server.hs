module Domain.Server (runApp) where

import Api.Backend
import App
import Control.Monad.Except (ExceptT (..))
import Network.Wai.Handler.Warp (run)
import Servant
import TePapa.Env (getPort, loadDotEnv)

nt :: AppEnv -> AppM a -> Handler a
nt env appAction = Handler . ExceptT $ (Right <$> runAppM appAction env)

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

server :: ServerT BackendApi AppM
server = serveStart :<|> serveExpand

serveExpand :: ExpandParams -> AppM [ClientGraphAction]
serveExpand params = error "todo"

serveStart :: AppM InitialGameState
serveStart = error "todo"
