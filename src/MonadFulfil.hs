{-# LANGUAGE TypeFamilies #-}

module MonadFulfil (MonadFulfil (..)) where

import Control.Concurrent.STM
import Data.Kind

class (Monad m) => MonadFulfil m where
    type Fulfil m :: Type -> Type
    fulfil :: Fulfil m a -> a -> m ()

instance MonadFulfil STM where
    type Fulfil STM = TMVar
    fulfil = writeTMVar
