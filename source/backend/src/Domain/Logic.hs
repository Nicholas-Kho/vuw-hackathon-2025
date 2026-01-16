{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Logic (
    DrunkardsWalkSettings (..),
    drunkardsWalk,
    fetchNode,
)
where

import Cache.Interface
import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.List.NonEmpty as N
import Data.Maybe
import qualified Data.Set as S
import Domain.Model
import MonadFulfil
import TePapa.Client
import TePapa.Convert

stitchFragment ::
    ( GraphStore g
    , MonadFulfil (StoreM g)
    ) =>
    g ->
    NodeId ->
    Fulfil (StoreM g) Node ->
    GraphFragment ->
    StoreM g ()
stitchFragment g nid obligation frag = do
    fulfil obligation (Ok . content $ frag)
    forM_ (outEdges frag) (link g . mkEdgeFrom nid)
    forM_ (inEdges frag) (link g . mkEdgeTo nid)

fetchNode ::
    ( GraphStore g
    , ApiM m
    , MonadWait (StoreM g)
    , MonadFulfil (StoreM g)
    ) =>
    (forall a. StoreM g a -> m a) ->
    g ->
    NodeId ->
    m (Node)
fetchNode liftSM graph nid = do
    (liftSM $ claimFetch graph nid) >>= \case
        AlreadyThere n -> pure n
        WaitForResult h -> liftSM $ await h
        Proceed obligation -> do
            (runReq $ fetchReference nid) >>= \case
                Left cerr -> do
                    liftSM $ fulfil obligation (Fail cerr)
                    pure (Fail cerr)
                Right fragment -> do
                    liftSM $ stitchFragment graph nid obligation fragment
                    pure . Ok . content $ fragment

data DrunkardsWalkSettings = DrunkardsWalkSettings
    { numSteps :: Int
    , start :: NodeId
    }

data DrunkardsWalkState = DrunkardsWalkState
    { stepsLeft :: Int
    , path :: N.NonEmpty NodeId
    , visited :: S.Set NodeId
    }

initialDrunkardsState :: DrunkardsWalkSettings -> DrunkardsWalkState
initialDrunkardsState settings =
    DrunkardsWalkState
        { stepsLeft = numSteps settings
        , path = N.singleton . start $ settings
        , visited = S.empty
        }

drunkardsWalk ::
    ( GraphStore g
    , MonadWait (StoreM g)
    , MonadFulfil (StoreM g)
    , ApiM m
    , MonadRandom m
    ) =>
    (forall a. StoreM g a -> m a) ->
    g ->
    DrunkardsWalkSettings ->
    m NodeId
drunkardsWalk liftSM g settings = do
    _ <- fetchNode liftSM g (start settings)
    evalStateT (drunkardsWalkHelper liftSM g) (initialDrunkardsState settings)

randomItem :: (MonadRandom m) => N.NonEmpty a -> m a
randomItem ne = do
    randomIndex <- getRandomR (0, N.length ne - 1)
    pure (ne N.!! randomIndex)

setToNE :: S.Set a -> Maybe (N.NonEmpty a)
setToNE = N.nonEmpty . S.elems

-- PERF: Maybe instead of loading each edge one at a time and trying the next
-- if we fail, we bulk load all the edges concurrently, then pick a random
-- succesful one if possible?
fetchRandomNeighbor ::
    ( GraphStore g
    , MonadWait (StoreM g)
    , MonadFulfil (StoreM g)
    , MonadRandom m
    , ApiM m
    ) =>
    S.Set NodeId ->
    (forall a. StoreM g a -> m a) ->
    g ->
    NodeId ->
    m (Maybe (NodeContent, NodeId))
fetchRandomNeighbor triedSoFar liftSM graph nid = do
    edges <- liftSM $ fromMaybe S.empty <$> outgoingEdges graph nid
    let candidates = (S.map (\e -> e.to) edges) `S.difference` triedSoFar
    case setToNE candidates of
        Nothing -> pure Nothing
        Just ne -> do
            neighId <- randomItem ne
            fetchNode liftSM graph neighId >>= \case
                Ok content -> pure . Just $ (content, neighId)
                Fail _ ->
                    fetchRandomNeighbor (S.insert neighId triedSoFar) liftSM graph nid

drunkardsWalkHelper ::
    ( GraphStore g
    , MonadWait (StoreM g)
    , MonadFulfil (StoreM g)
    , ApiM m
    , MonadRandom m
    ) =>
    (forall a. StoreM g a -> m a) ->
    g ->
    StateT DrunkardsWalkState m NodeId
drunkardsWalkHelper liftSM graph = do
    currentPath <- gets path
    alreadyVisited <- gets visited
    let currentNode = N.head currentPath
    gets stepsLeft >>= \case
        x | x <= 0 -> pure currentNode
        _positive -> do
            nextNode <-
                lift $
                    fetchRandomNeighbor
                        alreadyVisited
                        liftSM
                        graph
                        currentNode
            case nextNode of
                Nothing -> pure currentNode
                Just (_, nextId) -> do
                    modify'
                        ( \s ->
                            s
                                { path = nextId N.<| currentPath
                                , stepsLeft = stepsLeft s - 1
                                , visited = S.insert nextId (visited s)
                                }
                        )
                    drunkardsWalkHelper liftSM graph
