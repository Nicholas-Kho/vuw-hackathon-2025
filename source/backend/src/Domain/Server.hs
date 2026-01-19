module Domain.Server () where

import Api.Backend
import App
import Control.Monad.Except (ExceptT (..))
import Servant

nt :: AppEnv -> AppM a -> Handler a
nt env appAction = Handler . ExceptT $ (Right <$> runAppM appAction env)

app :: AppEnv -> Application
app appEnv =
    serve (Proxy @BackendApi) $
        hoistServer (Proxy @BackendApi) (nt appEnv) server

server :: ServerT BackendApi AppM
server = serveStart :<|> serveExpand

serveExpand :: ExpandParams -> AppM [ClientGraphAction]
serveExpand params = error "todo"

serveStart :: AppM InitialGameState
serveStart = error "todo"
