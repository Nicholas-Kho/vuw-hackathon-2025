{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Logic (randomFromStore)
where

import App (AppEnv (graph), AppM)
import Cache.Interface (GraphStore (getNode), getKeys)
import Cache.NodeId (NodeId)
import Control.Monad.Random.Strict (MonadIO (liftIO), MonadRandom (getRandomR))
import Control.Monad.Reader (asks)
import qualified Data.List.NonEmpty as N
import qualified Data.Set as S
import Domain.Model (Node)
import GHC.Conc (atomically)

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
