{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module FetchStore.TePapaFetchStore (Store, emptyStore) where

import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as M
import FetchStore.Interface
import Servant.Client (ClientError)
import TePapa.CommonObject (TePapaThing)
import TePapa.Decode (RelatedThings, TePapaReference)
import TePapa.Traverse (FetchReq (..))

data Store = Store
    { things :: M.Map TePapaReference (TMVar (Either ClientError TePapaThing))
    , related :: M.Map TePapaReference (TMVar (Either ClientError RelatedThings))
    }

instance FetchStore FetchReq Store where
    type StoreM Store = STM
    type WaitType Store = TMVar
    type ObligationType Store = TMVar
    claimFetch store req =
        case req of
            GetId tref -> claimFetchGetId store tref
            GetRelated tref -> claimFetchGetRelated store tref

claimFetchGetRelated :: Store -> TePapaReference -> STM (FetchResult TMVar TMVar (Either ClientError RelatedThings))
claimFetchGetRelated store tref = do
    (M.lookup tref store.related) >>= \case
        Nothing -> do
            writeHere <- newEmptyTMVar
            M.insert tref writeHere (store.related)
            pure (StartFetch writeHere)
        Just tmv ->
            tryReadTMVar tmv >>= \case
                Nothing -> pure (Waiting tmv)
                Just x -> pure (Cached x)

claimFetchGetId :: Store -> TePapaReference -> STM (FetchResult TMVar TMVar (Either ClientError TePapaThing))
claimFetchGetId store tref =
    (M.lookup tref store.things) >>= \case
        Nothing -> do
            writeHere <- newEmptyTMVar
            M.insert tref writeHere (store.things)
            pure (StartFetch writeHere)
        Just tmv ->
            tryReadTMVar tmv >>= \case
                Nothing -> pure (Waiting tmv)
                Just x -> pure (Cached x)

emptyStore :: STM Store
emptyStore = do
    emptyThings <- M.empty
    emptyRelated <- M.empty
    pure (Store{things = emptyThings, related = emptyRelated})
