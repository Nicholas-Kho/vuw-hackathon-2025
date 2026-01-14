module Domain.Logic () where

import Cache.Interface
import Domain.Model
import Servant.Client (ClientError)
import TePapa.Client
import TePapa.Convert

fetchNode ::
    ( GraphStore g
    , ApiM m
    ) =>
    (forall a. StoreM g a -> m a) ->
    g ->
    NodeId ->
    m (Either ClientError NodeContent)
fetchNode liftSM graph nid =
    liftSM (tryFetch graph nid) >>= \case
        AlreadyThere r -> pure r
        WaitForResult -> error "what now?"
        Proceed -> do
            liftSM $ writeNode graph nid Fetching
            res <- runReq $ fetchReference nid
            case res of
                Left cerr ->
                    liftSM $ failNode graph nid cerr >> pure (Left cerr)
                Right frag ->
                    liftSM $ stitchFragment graph nid frag >> pure (Right . content $ frag)
