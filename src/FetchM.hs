module FetchM (FetchM, fork, fetch, runFetch, runFetchConc) where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception (bracket_)
import Control.Monad (forM)
import Control.Monad.Free
import FetchStore.Interface
import MonadFulfil
import MonadWait

data Fetch req a where
    WithReq :: req v -> (v -> a) -> Fetch req a
    Fork :: [FetchM req v] -> ([v] -> a) -> Fetch req a

instance Functor (Fetch req) where
    fmap f (WithReq query mkOp) = WithReq query (f . mkOp)
    fmap f (Fork comps mkOpFromResults) = Fork comps (f . mkOpFromResults)

type FetchM req = Free (Fetch req)

fetch :: req a -> FetchM req a
fetch r = liftF (WithReq r id)

fork :: [FetchM req a] -> FetchM req [a]
fork comps = liftF (Fork comps id)

runFetch :: (Monad m) => (forall t. req t -> m t) -> FetchM req a -> m a
runFetch _ (Pure a) = return a
runFetch runQuery (Free (WithReq query ansToNextComp)) = do
    result <- runQuery query
    let nextComp = ansToNextComp result
    runFetch runQuery nextComp
runFetch runQuery (Free (Fork queries answersToNextComp)) = do
    answers <- forM queries (runFetch runQuery)
    let nextComp = answersToNextComp answers
    runFetch runQuery nextComp

runFetchConc ::
    ( FetchStore req f
    , MonadWait (StoreM f)
    , MonadFulfil (StoreM f)
    ) =>
    QSem ->
    (forall t. StoreM f t -> IO t) ->
    (forall t. req t -> IO t) ->
    f ->
    FetchM req a ->
    IO a
runFetchConc qsem liftFM runReq store comp =
    case comp of
        Pure x -> pure x
        Free (WithReq req mkNext) -> do
            res <- runReqWithDedup qsem liftFM runReq store req
            runFetchConc qsem liftFM runReq store (mkNext res)
        Free (Fork reqs mkNext) -> do
            answers <- forConcurrently reqs (runFetchConc qsem liftFM runReq store)
            runFetchConc qsem liftFM runReq store (mkNext answers)

runReqWithDedup ::
    ( FetchStore req f
    , MonadWait (StoreM f)
    , MonadFulfil (StoreM f)
    ) =>
    QSem ->
    (forall t. StoreM f t -> IO t) ->
    (forall t. req t -> IO t) ->
    f ->
    req a ->
    IO a
runReqWithDedup qsem liftFM runReq store req =
    (liftFM $ claimFetch store req) >>= \case
        Cached x -> pure x
        Waiting h -> do
            res <- liftFM $ await h
            pure res
        StartFetch ob -> do
            res <- bracket_ (waitQSem qsem) (signalQSem qsem) (runReq req)
            liftFM $ fulfil ob res
            pure res
