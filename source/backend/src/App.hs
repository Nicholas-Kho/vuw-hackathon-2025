module App (
    AppEnv (..),
    AppM (..),
    getInitialEnv,
    runAppM,
)
where

import Api.TePapa
import Bootstrap (fetchSeed)
import Cache.Interface
import Cache.TVarGraphStore
import Control.Concurrent (QSem)
import Control.Monad.Random.Strict
import Control.Monad.Reader
import FetchStore.TePapaFetchStore
import GHC.Conc
import Network.HTTP.Client.TLS
import Servant.Client
import TePapa.Client
import TePapa.Env

data AppEnv = AppEnv
    { graph :: Graph
    , fetchStore :: Store
    , apiKey :: ApiKey
    , clientEnv :: ClientEnv
    , semaphore :: QSem
    }

newtype AppM a = AppM
    { unAppM :: ReaderT AppEnv IO a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

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
    loadDotEnv
    key <- getApiKey
    initialFetchStore <- atomically emptyStore
    env <- getClientEnv
    sem <- getSemaphore
    seed <- getSeed
    rootNode <- fetchSeed key env seed
    initialGraph <- atomically (initStore seed rootNode)
    pure $
        AppEnv
            { graph = initialGraph
            , apiKey = ApiKey key
            , clientEnv = env
            , semaphore = sem
            , fetchStore = initialFetchStore
            }

runAppM :: AppM a -> AppEnv -> IO a
runAppM action env = runReaderT (unAppM action) env
