{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Logic (pickRandomFromStore)
where

import Cache.Interface
import Control.Monad.Random.Strict (MonadRandom (getRandomR))
import Control.Monad.State.Strict
import qualified Data.List.NonEmpty as N
import qualified Data.Set as S
import Domain.Model

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

pickRandomFromStore ::
    ( MonadRandom m
    , GraphStore g
    , MonadWait (StoreM g)
    ) =>
    (forall t. StoreM g t -> m t) ->
    g ->
    m (Maybe (NodeId, Node))
pickRandomFromStore liftSM graph = do
    keys <- liftSM $ listKeys graph
    evalStateT (pickRandomHelper liftSM graph) keys

pickRandomHelper ::
    ( MonadRandom m
    , GraphStore g
    , MonadWait (StoreM g)
    ) =>
    (forall t. StoreM g t -> m t) ->
    g ->
    StateT (S.Set NodeId) m (Maybe (NodeId, Node))
pickRandomHelper liftSM store =
    get >>= randomFromSet >>= \case
        Nothing -> pure Nothing
        Just key -> do
            modify' (S.delete key)
            (lift . liftSM $ readNode store key) >>= \case
                Missing -> pickRandomHelper liftSM store
                ItWasThere x -> pure . Just $ (key, x)
                NeedToWait h ->
                    (lift . liftSM $ await h) >>= pure . Just . (key,)
