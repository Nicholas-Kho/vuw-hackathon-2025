module App (
    AppEnv (..),
    AppM (..),
    getInitialEnv,
    runAppM,
)
where

import Cache.Interface
import Cache.TVarGraphStore
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Data.Text
import GHC.Conc
import Network.HTTP.Client.TLS
import Servant.Client
import TePapa.Client
import TePapa.Env

data AppEnv = AppEnv
    { graph :: Graph
    , apiKey :: Text
    , clientEnv :: ClientEnv
    }

newtype AppM a = AppM
    { unAppM :: RandT StdGen (ReaderT AppEnv IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadRandom)

getClientEnv :: IO ClientEnv
getClientEnv = do
    manager <- newTlsManager
    pure $ mkClientEnv manager collectionsURL

getInitialEnv :: IO AppEnv
getInitialEnv = do
    key <- getApiKey
    initialGraph <- atomically blankGraph
    env <- getClientEnv
    pure $ AppEnv{graph = initialGraph, apiKey = key, clientEnv = env}

runAppM :: AppM a -> AppEnv -> IO a
runAppM action env = do
    gen <- newStdGen
    runReaderT (evalRandT (unAppM action) gen) env
