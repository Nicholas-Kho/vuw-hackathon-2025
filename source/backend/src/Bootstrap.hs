{-# LANGUAGE OverloadedRecordDot #-}

module Bootstrap (fetchSeed) where

import Api.TePapa (ApiKey (ApiKey))
import Cache.NodeId (NodeId)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, ask, runReaderT)
import qualified Data.Text as T
import Domain.Model
import FetchM (runFetch)
import Servant.Client (ClientEnv, runClientM)
import System.Exit (die)
import TePapa.Client (ApiM (..))
import TePapa.Decode (TePapaReference)
import TePapa.Traverse (doQuery, getNodeById)

fetchSeed :: T.Text -> ClientEnv -> TePapaReference -> IO NodeContent
fetchSeed key env seed = runBootstrapM (key, env) (fetchSeedHelp seed)

fetchSeedHelp :: TePapaReference -> BootstrapM NodeContent
fetchSeedHelp seed = do
    disc <- runFetch doQuery (getNodeById seed)
    error "todo"

newtype BootstrapM a = BootstrapM
    {unBootstrapM :: ReaderT (T.Text, ClientEnv) IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (T.Text, ClientEnv))

instance ApiM BootstrapM where
    runReq r = do
        (key, env) <- ask
        liftIO $ runClientM (r . ApiKey $ key) env

runBootstrapM :: (T.Text, ClientEnv) -> BootstrapM a -> IO a
runBootstrapM params BootstrapM{unBootstrapM = action} = runReaderT action params
