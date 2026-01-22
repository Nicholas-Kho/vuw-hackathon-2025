{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Logic ()
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
import TePapa.Traverse (Discovery, TFetch, doQuery, getNodeById)

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
