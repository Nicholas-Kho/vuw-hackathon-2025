{-# LANGUAGE TypeFamilies #-}

module MonadWait (MonadWait (..)) where

import Control.Concurrent.STM
import Data.Kind

class (Monad m) => MonadWait m where
    type Wait m :: Type -> Type
    await :: Wait m a -> m a

instance MonadWait STM where
    type Wait STM = TMVar
    await = readTMVar
