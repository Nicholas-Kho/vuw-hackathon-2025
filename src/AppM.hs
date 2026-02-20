module AppM (
    AppEnv (..),
    AppM (..),
    runAppM,
    setupApp,
)
where

import AiSummary.DescriptionQueue (DescriptionHeap, newDescHeap)
import AiSummary.LlamaApi (llamaUrl)
import AiSummary.StartLlama (startLlamaWaitForReady)
import Api.TePapa
import Bootstrap (fetchSeed)
import Cache.Interface
import Cache.TVarGraphStore
import Control.Concurrent (QSem)
import Control.Monad.Random.Strict
import Control.Monad.Reader
import Env
import FetchStore.TePapaFetchStore
import GHC.Conc
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS
import Servant.Client
import TePapa.Client

data AppEnv = AppEnv
    { graph :: Graph
    , fetchStore :: Store
    , apiKey :: ApiKey
    , clientEnvCollections :: ClientEnv
    , clientEnvLlama :: ClientEnv
    , semaphore :: QSem
    , descriptionQueue :: DescriptionHeap
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
    port <- portToLlamaOn
    manager <- Http.newManager Http.defaultManagerSettings
    pure $ mkClientEnv manager (llamaUrl port)

setupApp :: IO AppEnv
setupApp = do
    loadDotEnv
    key <- getApiKey
    initialFetchStore <- atomically emptyStore
    emptyQueue <- atomically newDescHeap
    envCollections <- makeClientEnvCollections
    envLlama <- makeClientEnvLlama
    sem <- getSemaphore
    seed <- getSeed
    _llamaHandle <- startLlamaWaitForReady envLlama
    rootNode <- fetchSeed key envCollections seed
    initialGraph <- atomically (initStore seed rootNode)
    pure $
        AppEnv
            { graph = initialGraph
            , apiKey = ApiKey key
            , clientEnvCollections = envCollections
            , clientEnvLlama = envLlama
            , semaphore = sem
            , fetchStore = initialFetchStore
            , descriptionQueue = emptyQueue
            }

runAppM :: AppM a -> AppEnv -> IO a
runAppM action env = runReaderT (unAppM action) env
