{-# LANGUAGE TypeFamilies #-}

module FetchStore.Interface (FetchStore (..), FetchResult (..)) where

import Data.Kind

data FetchResult w o a
    = Cached a
    | Waiting (w a)
    | StartFetch (o a)

class (Monad (StoreM f)) => FetchStore (req :: Type -> Type) f where
    -- Monadic context for store operations
    type StoreM f :: Type -> Type

    -- Type used when waiting for in-flight requests
    type WaitType f :: Type -> Type

    -- Type to fulfil a fetch when you perform it yourself
    type ObligationType f :: Type -> Type
    claimFetch :: f -> req a -> StoreM f (FetchResult (WaitType f) (ObligationType f) a)
