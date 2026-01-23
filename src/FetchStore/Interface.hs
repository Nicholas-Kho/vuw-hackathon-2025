{-# LANGUAGE TypeFamilies #-}

module FetchStore.Interface (FetchStore (..), FetchResult (..)) where

import Data.Kind
import MonadFulfil
import MonadWait

data FetchResult w o a
    = Cached a
    | Waiting (w a)
    | StartFetch (o a)

class (Monad (StoreM f)) => FetchStore (req :: Type -> Type) f where
    -- Monadic context for store operations
    type StoreM f :: Type -> Type
    claimFetch :: f -> req a -> StoreM f (FetchResult (Wait (StoreM f)) (Fulfil (StoreM f)) a)
