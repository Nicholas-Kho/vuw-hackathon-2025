module FetchM (FetchM, fetch, runFetch) where

import Control.Monad.Free

data Fetch k v next
    = WithLookup k (v -> next)
    deriving (Functor)

type FetchM k v = Free (Fetch k v)

fetch :: k -> FetchM k v v
fetch k = liftF (WithLookup k id)

runFetch :: (Monad m) => (k -> m v) -> FetchM k v a -> m a
runFetch _ (Pure a) = return a
runFetch get (Free (WithLookup key mkNextComp)) = do
    value <- get key
    let nextComp = mkNextComp value
    runFetch get nextComp
