{-# LANGUAGE OverloadedRecordDot #-}

module Bootstrap (fetchSeed) where

import Api.TePapa (ApiKey (ApiKey))
import Cache.Interface (GraphAction (..))
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, ask, runReaderT)
import qualified Data.Text as T
import Domain.Model
import FetchM (runFetch)
import Servant.Client (ClientEnv, runClientM)
import System.Exit (die)
import TePapa.Client (ApiM (..))
import TePapa.Convert (discoveryToAction)
import TePapa.Decode (TePapaReference)
import TePapa.Traverse (doQuery, getNodeById)

fetchSeed :: T.Text -> ClientEnv -> TePapaReference -> IO (NodeId, Node)
fetchSeed key env seed = runBootstrapM (key, env) (fetchSeedHelp seed)

fetchSeedHelp :: TePapaReference -> BootstrapM (NodeId, Node)
fetchSeedHelp seed = do
    disc <- runFetch doQuery (getNodeById seed)
    let acts = discoveryToAction disc
    case lookForGraphAdd acts of
        Just (nid, node) -> pure (nid, node)
        -- TODO: Maybe a more detailed error message
        -- (Was the failure a network issue, a JSON decoding issue, or a node
        -- conversion issue?)
        Nothing -> liftIO $ die ("Could not fetch the seed " <> (show seed) <> " as a node.")

lookForGraphAdd :: [GraphAction] -> Maybe (NodeId, Node)
lookForGraphAdd [] = Nothing
lookForGraphAdd ((AddEdge _) : xs) = lookForGraphAdd xs
lookForGraphAdd ((AddNode nid n) : _) = Just (nid, n)

newtype BootstrapM a = BootstrapM
    {unBootstrapM :: ReaderT (T.Text, ClientEnv) IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (T.Text, ClientEnv))

instance ApiM BootstrapM where
    runReq r = do
        (key, env) <- ask
        liftIO $ runClientM (r . ApiKey $ key) env

runBootstrapM :: (T.Text, ClientEnv) -> BootstrapM a -> IO a
runBootstrapM params BootstrapM{unBootstrapM = action} = runReaderT action params
