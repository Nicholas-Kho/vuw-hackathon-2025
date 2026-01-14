module Domain.Logic (fetchNode) where

import Cache.Interface
import Control.Monad
import Domain.Model
import MonadFulfil
import TePapa.Client
import TePapa.Convert

stitchFragment ::
    ( GraphStore g
    , MonadFulfil (StoreM g)
    ) =>
    g ->
    NodeId ->
    Fulfil (StoreM g) Node ->
    GraphFragment ->
    StoreM g ()
stitchFragment g nid obligation frag = do
    fulfil obligation (Ok . content $ frag)
    forM_ (outEdges frag) (link g . partEdgeFrom nid)
    forM_ (inEdges frag) (link g . partEdgeTo nid)

fetchNode ::
    ( GraphStore g
    , ApiM m
    , MonadWait (StoreM g)
    , MonadFulfil (StoreM g)
    ) =>
    (forall a. StoreM g a -> m a) ->
    g ->
    NodeId ->
    m (Node)
fetchNode liftSM graph nid = do
    (liftSM $ claimFetch graph nid) >>= \case
        AlreadyThere n -> pure n
        WaitForResult h -> liftSM $ await h
        Proceed obligation -> do
            (runReq $ fetchReference nid) >>= \case
                Left cerr -> do
                    liftSM $ fulfil obligation (Fail cerr)
                    pure (Fail cerr)
                Right fragment -> do
                    let newNode = Ok . content $ fragment
                    liftSM $ stitchFragment graph nid obligation fragment
                    pure newNode
