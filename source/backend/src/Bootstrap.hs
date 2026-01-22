{-# LANGUAGE OverloadedRecordDot #-}

module Bootstrap (fetchSeed) where

import Api.TePapa (ApiKey (ApiKey))
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, ask, runReaderT)
import qualified Data.Text as T
import Domain.Model
import FetchM (runFetch)
import Servant.Client (ClientEnv, runClientM)
import System.Exit (die)
import TePapa.Client (ApiM (..))
import TePapa.CommonObject (prettyPrintThing)
import TePapa.Convert (tePapaThingToNode)
import TePapa.Decode (TePapaReference)
import TePapa.Traverse (Discovery (..), doQuery, getNodeById)

fetchSeed :: T.Text -> ClientEnv -> TePapaReference -> IO NodeContent
fetchSeed key env seed = runBootstrapM (key, env) (fetchSeedHelp seed)

fetchSeedHelp :: TePapaReference -> BootstrapM NodeContent
fetchSeedHelp seed = do
    disc <- runFetch doQuery (getNodeById seed)
    tthing <- case disc of
        FoundThing _ t -> pure t
        ErrorFetching tref cerr ->
            liftIO . die $
                "Couldn't bootstrap because of error fetching "
                    <> (show tref)
                    <> ": "
                    <> (show cerr)
        FoundLink _ _ _ -> liftIO . die $ "Couldn't bootstrap: found a link instead of an object."
    case tePapaThingToNode tthing of
        Nothing -> liftIO . die $ "Couln't bootstrap: Can't convert " <> (prettyPrintThing tthing) <> " to NodeContent."
        Just ncon -> pure (ncon)

newtype BootstrapM a = BootstrapM
    {unBootstrapM :: ReaderT (T.Text, ClientEnv) IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (T.Text, ClientEnv))

instance ApiM BootstrapM where
    runReq r = do
        (key, env) <- ask
        liftIO $ runClientM (r . ApiKey $ key) env

runBootstrapM :: (T.Text, ClientEnv) -> BootstrapM a -> IO a
runBootstrapM params BootstrapM{unBootstrapM = action} = runReaderT action params
