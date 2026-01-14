module App (
    AppEnv (..),
    AppM (..),
    getInitialEnv,
    runAppM,
)
where

import Api.TePapa
import Cache.Interface
import Cache.TVarGraphStore
import Control.Monad.Random.Strict
import Control.Monad.Reader
import GHC.Conc
import Network.HTTP.Client.TLS
import Servant.Client
import TePapa.Client
import TePapa.Env

data AppEnv = AppEnv
    { graph :: Graph
    , apiKey :: ApiKey
    , clientEnv :: ClientEnv
    }

newtype AppM a = AppM
    { unAppM :: RandT StdGen (ReaderT AppEnv IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadRandom)

instance ApiM AppM where
    runReq needsKey = do
        key <- asks apiKey
        cenv <- asks clientEnv
        liftIO $ runClientM (needsKey key) cenv

getClientEnv :: IO ClientEnv
getClientEnv = do
    manager <- newTlsManager
    pure $ mkClientEnv manager collectionsURL

getInitialEnv :: IO AppEnv
getInitialEnv = do
    key <- getApiKey
    initialGraph <- atomically blankGraph
    env <- getClientEnv
    pure $ AppEnv{graph = initialGraph, apiKey = ApiKey key, clientEnv = env}

runAppM :: AppM a -> AppEnv -> IO a
runAppM action env = do
    gen <- newStdGen
    runReaderT (evalRandT (unAppM action) gen) env
