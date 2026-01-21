{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Logic (pickRandomFromStore)
where

import App (AppEnv (graph), AppM, fetchStore, runAppM, semaphore)
import Cache.Interface
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_)
import Control.Monad.Random.Strict (MonadRandom (getRandomR))
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Strict
import qualified Data.List.NonEmpty as N
import qualified Data.Set as S
import Domain.Model
import FetchM (runFetchConc)
import TePapa.Convert (discoveryToAction)
import TePapa.Traverse (Discovery, TFetch, doQuery)

randomFromNEL :: (MonadRandom m) => N.NonEmpty a -> m a
randomFromNEL nel = do
    randomIndex <- getRandomR (0, N.length nel - 1)
    pure (nel N.!! randomIndex)

setToNEL :: S.Set a -> Maybe (N.NonEmpty a)
setToNEL = N.nonEmpty . S.elems

randomFromSet :: (MonadRandom m) => S.Set a -> m (Maybe a)
randomFromSet s =
    case setToNEL s of
        Nothing -> pure Nothing
        Just x -> Just <$> randomFromNEL x

pickRandomFromStore ::
    ( MonadRandom m
    , GraphStore g
    , MonadWait (StoreM g)
    ) =>
    (forall t. StoreM g t -> m t) ->
    g ->
    m (NodeId, Node)
pickRandomFromStore liftSM graph = do
    keys <- liftSM $ listKeys graph
    evalStateT (pickRandomHelper liftSM graph) keys >>= \case
        Nothing -> liftSM $ getRoot graph
        Just r -> pure r

pickRandomHelper ::
    ( MonadRandom m
    , GraphStore g
    , MonadWait (StoreM g)
    ) =>
    (forall t. StoreM g t -> m t) ->
    g ->
    StateT (S.Set NodeId) m (Maybe (NodeId, Node))
pickRandomHelper liftSM store =
    get >>= randomFromSet >>= \case
        Nothing -> pure Nothing
        Just key -> do
            modify' (S.delete key)
            (lift . liftSM $ readNode store key) >>= \case
                Missing -> pickRandomHelper liftSM store
                ItWasThere x -> pure . Just $ (key, x)
                NeedToWait h ->
                    (lift . liftSM $ await h) >>= pure . Just . (key,)

doTFetch :: TFetch a -> AppM a
doTFetch fetch = do
    env <- ask
    let fstore = fetchStore env
    let sem = semaphore env
    let doQueryIO e r = doQuery r `runAppM` e
    liftIO $ runFetchConc sem (liftIO . atomically) (doQueryIO env) fstore fetch

processDiscovery :: Discovery -> AppM [GraphAction]
processDiscovery d = do
    let acts = discoveryToAction d
    store <- asks graph
    forM_ acts $ liftIO . atomically . runAction store
    pure acts

processDiscoveriesConc :: [Discovery] -> AppM [GraphAction]
processDiscoveriesConc ds = do
    e <- ask
    let processDiscoveryIO env d = (processDiscovery d) `runAppM` env
    -- TODO: Proper error handling if a thread dies!
    liftIO $ concat <$> mapConcurrently (processDiscoveryIO e) ds
