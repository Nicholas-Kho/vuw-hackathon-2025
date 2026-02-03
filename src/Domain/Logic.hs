{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Logic (
    drunkardsWalk,
    expandNode,
    lookupNodes,
    randomFromStore,
    verifyNodeId,
)
where

import App (AppEnv (..), AppM, runAppM)
import Cache.Interface (GraphStore (getNode), addEdge, addNode, getExternal, getKeys)
import Cache.NodeId (NodeId, mkNodeId)
import Control.Concurrent.Async (forConcurrently, forConcurrently_)
import Control.Monad.Random.Strict (MonadIO (liftIO), MonadRandom (getRandomR), lift)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify')
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M
import qualified Data.Set as S
import Domain.Model (EdgeInfo (..), Node (outgoingEdges))
import FetchM (runFetchConc)
import GHC.Conc (atomically)
import TePapa.Convert (edgeReasonToTxt, tePapaThingToNode)
import TePapa.Traverse (Discovery (..), TFetch, doQuery, getNeighs)

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

randomFromStore :: (MonadRandom m) => (forall t. AppM t -> m t) -> m (NodeId, Node)
randomFromStore liftAppM = do
    store <- liftAppM $ asks graph
    (rootKey, storeKeys) <- liftAppM . liftIO . atomically . getKeys $ store
    nid <-
        randomFromSet storeKeys >>= \case
            Nothing -> pure rootKey
            Just k -> pure k
    node <- liftAppM . liftIO . atomically $ getNode store nid
    pure (nid, node)

expandNode :: NodeId -> AppM (S.Set NodeId)
expandNode nid = do
    store <- asks graph
    node <- liftIO . atomically $ getNode store nid
    if S.null . M.keysSet . outgoingEdges $ node
        then do
            eid <- liftIO . atomically $ getExternal store nid
            discoveries <- runTFetch $ getNeighs eid
            processDiscoveries discoveries
            node' <- liftIO . atomically $ getNode store nid
            pure . M.keysSet . outgoingEdges $ node'
        else do
            pure . M.keysSet . outgoingEdges $ node

data DrunkardsWalkState = DrunkardsWalkState
    { visited :: S.Set NodeId
    , path :: N.NonEmpty NodeId
    , stepsLeft :: Int
    }

drunkardsWalkHelper ::
    ( MonadRandom m
    ) =>
    (forall t. AppM t -> m t) ->
    StateT DrunkardsWalkState m (N.NonEmpty NodeId)
drunkardsWalkHelper liftAppM = do
    steps <- gets stepsLeft
    curPath <- gets path
    v <- gets visited
    if steps <= 0
        then pure . N.reverse $ curPath
        else do
            let comingFrom = N.head curPath
            adj <- lift . liftAppM $ expandNode comingFrom
            let adjFilter = adj S.\\ v
            (lift $ randomFromSet adjFilter) >>= \case
                Nothing -> pure . N.reverse $ curPath
                Just nextId -> do
                    modify'
                        ( \s ->
                            s
                                { stepsLeft = (stepsLeft s) - 1
                                , visited = S.insert nextId v
                                , path = nextId N.<| curPath
                                }
                        )
                    drunkardsWalkHelper liftAppM

drunkardsWalk ::
    (MonadRandom m) =>
    (forall t. AppM t -> m t) ->
    NodeId ->
    Int ->
    m (N.NonEmpty (NodeId, Node))
drunkardsWalk liftAppM start steps = do
    let initialState =
            DrunkardsWalkState
                { visited = S.singleton start
                , path = N.singleton start
                , stepsLeft = steps
                }
    path <- evalStateT (drunkardsWalkHelper liftAppM) initialState
    liftAppM $ lookupNodes path

lookupNodes :: (Traversable t) => t NodeId -> AppM (t (NodeId, Node))
lookupNodes nids = do
    g <- asks graph
    env <- ask
    let lookupNode = liftIO . atomically . getNode g
    let lookupNodeIO e nid = (lookupNode nid) `runAppM` e
    liftIO . forConcurrently nids $ \nid -> do
        node <- lookupNodeIO env nid
        pure (nid, node)

runTFetch :: TFetch a -> AppM a
runTFetch f = do
    sem <- asks semaphore
    store <- asks fetchStore
    env <- ask
    let doQueryIO e req = (doQuery req) `runAppM` e
    liftIO $ runFetchConc sem atomically (doQueryIO env) store f

processDiscovery :: Discovery -> AppM ()
processDiscovery d = do
    g <- asks graph
    case d of
        ErrorFetching _ _ -> pure ()
        FoundThing tref thing -> case tePapaThingToNode thing of
            Nothing -> pure ()
            Just c -> liftIO . atomically $ addNode g tref c >> pure ()
        FoundLink t1 t2 why -> do
            liftIO . atomically $ addEdge g t1 t2 (EdgeInfo $ edgeReasonToTxt why)

processDiscoveries :: [Discovery] -> AppM ()
processDiscoveries ds = do
    env <- ask
    let processDiscoveryIO e d = (processDiscovery d) `runAppM` e
    liftIO $ forConcurrently_ ds (processDiscoveryIO env)

verifyNodeId :: Int -> AppM (Maybe NodeId)
verifyNodeId x = do
    g <- asks graph
    (rootKey, rest) <- liftIO . atomically $ getKeys g
    let allKeys = S.insert rootKey rest
    if (mkNodeId x) `S.member` allKeys
        then pure . Just . mkNodeId $ x
        else pure Nothing
