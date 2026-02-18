module App (
    AppEnv (..),
    AppM (..),
    runAppM,
    setupApp,
)
where

import AiSummary.LlamaApi (llamaUrl)
import AiSummary.StartLlama (startLlamaWaitForReady)
import Api.TePapa
import Bootstrap (fetchSeed)
import Cache.Interface
import Cache.TVarGraphStore
import Control.Concurrent (QSem)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Random.Strict
import Control.Monad.Reader
import FetchStore.TePapaFetchStore
import GHC.Conc
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS
import Servant.Client
import TePapa.Client
import TePapa.Env

data AppEnv = AppEnv
    { graph :: Graph
    , fetchStore :: Store
    , apiKey :: ApiKey
    , clientEnvCollections :: ClientEnv
    , clientEnvLlama :: ClientEnv
    , semaphore :: QSem
    }

newtype AppM a = AppM
    { unAppM :: ReaderT AppEnv IO a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

instance ApiM AppM where
    runReq needsKey = do
        key <- asks apiKey
        cenv <- asks clientEnvCollections
        liftIO $ runClientM (needsKey key) cenv

makeClientEnvCollections :: IO ClientEnv
makeClientEnvCollections = do
    manager <- newTlsManager
    pure $ mkClientEnv manager collectionsURL

makeClientEnvLlama :: IO ClientEnv
makeClientEnvLlama = do
    -- we are using HTTP here beacuse the llama server is running locally
    -- and is not exposed to the internet.
    manager <- Http.newManager Http.defaultManagerSettings
    -- TODO: make this port configurable!
    pure $ mkClientEnv manager (llamaUrl 8081)

setupApp :: IO AppEnv
setupApp = do
    loadDotEnv
    key <- getApiKey
    initialFetchStore <- atomically emptyStore
    envCollections <- makeClientEnvCollections
    envLlama <- makeClientEnvLlama
    sem <- getSemaphore
    seed <- getSeed
    (rootNode, _llamaHandle) <-
        concurrently
            (fetchSeed key envCollections seed)
            (startLlamaWaitForReady envLlama)
    initialGraph <- atomically (initStore seed rootNode)
    pure $
        AppEnv
            { graph = initialGraph
            , apiKey = ApiKey key
            , clientEnvCollections = envCollections
            , clientEnvLlama = envLlama
            , semaphore = sem
            , fetchStore = initialFetchStore
            }

runAppM :: AppM a -> AppEnv -> IO a
runAppM action env = runReaderT (unAppM action) env
