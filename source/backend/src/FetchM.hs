module FetchM (FetchM, fetch, runFetch) where

import Control.Monad.Free

data Fetch req a where
    WithReq :: req v -> (v -> a) -> Fetch req a

instance Functor (Fetch req) where
    fmap f (WithReq query mkOp) = WithReq query (f . mkOp)

type FetchM req = Free (Fetch req)

fetch :: req a -> FetchM req a
fetch r = liftF (WithReq r id)

runFetch :: (Monad m) => (forall t. req t -> m t) -> FetchM req a -> m a
runFetch _ (Pure a) = return a
runFetch runQuery (Free (WithReq query ansToNextComp)) = do
    result <- runQuery query
    let nextComp = ansToNextComp result
    runFetch runQuery nextComp
