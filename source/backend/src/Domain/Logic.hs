module Domain.Logic () where

import Cache.Interface
import Domain.Model
import Servant.Client (ClientError)
import TePapa.Client
import TePapa.Convert

fetchNode ::
    ( GraphStore g
    , ApiM m
    , MonadWait (StoreM g)
    ) =>
    (forall a. StoreM g a -> m a) ->
    g ->
    NodeId ->
    m (Either ClientError NodeContent)
fetchNode liftSM graph nid = error "todo"
