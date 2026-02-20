{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bootstrap (fetchSeed) where

import Api.TePapa (ApiKey (ApiKey))
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, asks, runReaderT)
import qualified Data.Text as T
import Domain.Model
import FetchM (runFetch)
import Servant.Client (ClientEnv, runClientM)
import System.Exit (die)
import TePapa.Client (ApiM (..))
import TePapa.Convert (tePapaThingToNode)
import TePapa.ExternalId (TePapaReference)
import TePapa.Traverse (Discovery (..), doQuery, getNodeById)

fetchSeed :: T.Text -> ClientEnv -> TePapaReference -> IO NodeContent
fetchSeed key cenv seed =
    let
        benv = BootstrapEnv key cenv
     in
        runBootstrapM benv (fetchSeedHelp seed)

fetchSeedHelp :: TePapaReference -> BootstrapM NodeContent
fetchSeedHelp seed = do
    disc <- runFetch doQuery (getNodeById seed)
    case disc of
        FoundThing _ t -> return $ tePapaThingToNode t
        ErrorFetching tref cerr ->
            liftIO . die $
                "Couldn't bootstrap because of error fetching "
                    <> (show tref)
                    <> ": "
                    <> (show cerr)
        FoundLink _ _ _ -> liftIO . die $ "Couldn't bootstrap: found a link instead of an object."

data BootstrapEnv = BootstrapEnv
    { key :: T.Text
    , cenvCollections :: ClientEnv
    }

newtype BootstrapM a = BootstrapM
    {unBootstrapM :: ReaderT BootstrapEnv IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader BootstrapEnv)

instance ApiM BootstrapM where
    runReq r = do
        key <- asks key
        env <- asks cenvCollections
        liftIO $ runClientM (r . ApiKey $ key) env

runBootstrapM :: BootstrapEnv -> BootstrapM a -> IO a
runBootstrapM benv BootstrapM{unBootstrapM = action} = runReaderT action benv
