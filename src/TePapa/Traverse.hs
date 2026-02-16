{-# LANGUAGE OverloadedRecordDot #-}

module TePapa.Traverse (
    CategoryInfo (..),
    Discovery (..),
    EdgeReason (..),
    FetchReq (..),
    TFetch,
    doQuery,
    getDirectNeighs,
    getNeighs,
    getNodeById,
    prettyPrintDiscovery,
) where

import qualified Data.Text as T
import FetchM (FetchM, fetch, forMFork)
import Servant.Client (ClientError)
import TePapa.Association (Association (..))
import TePapa.Client (ApiM (..), getById, getRelated)
import TePapa.CommonObject
import TePapa.ExternalId

data CategoryInfo = CategoryInfo
    { catTitle :: T.Text
    , catId :: Int
    }

data EdgeReason
    = Direct T.Text
    | ShareCategory CategoryInfo T.Text

data Discovery
    = FoundThing TePapaReference TePapaThing
    | ErrorFetching TePapaReference ClientError
    | FoundLink TePapaReference TePapaReference EdgeReason

prettyPrintEdgeReason :: EdgeReason -> String
prettyPrintEdgeReason r =
    case r of
        Direct t -> "direct neighbors via " <> (T.unpack t)
        ShareCategory cinf via -> "both " <> (T.unpack via) <> " " <> (T.unpack cinf.catTitle)

prettyPrintDiscovery :: Discovery -> String
prettyPrintDiscovery d =
    case d of
        ErrorFetching ref cerr -> showTePapaReferenceNice ref <> " FAILED: " <> show cerr
        FoundThing ref thing -> showTePapaReferenceNice ref <> " OK: " <> prettyPrintThing thing
        FoundLink ref1 ref2 why -> showTePapaReferenceNice ref1 <> " <-> " <> showTePapaReferenceNice ref2 <> " because " <> prettyPrintEdgeReason why

data FetchReq a where
    GetId :: TePapaReference -> FetchReq (Either ClientError TePapaThing)
    GetRelated :: TePapaReference -> FetchReq (Either ClientError [TePapaThing])

type TFetch a = FetchM FetchReq a

getNodeById :: TePapaReference -> TFetch Discovery
getNodeById tref = do
    res <- fetch (GetId tref)
    pure (responseToDiscovery tref res)

getNeighs :: TePapaReference -> TFetch [Discovery]
getNeighs = getDirectNeighs

getDirectNeighs ::
    TePapaReference ->
    TFetch [Discovery]
getDirectNeighs ofId = do
    outgoing <- fmap associations <$> fetch (GetId ofId)
    case outgoing of
        Left cerr -> pure [ErrorFetching ofId cerr]
        Right assocs -> concat <$> forMFork assocs (directNeighsFromAssoc ofId)

directNeighsFromAssoc ::
    TePapaReference ->
    Association ->
    TFetch [Discovery]
directNeighsFromAssoc comingFrom assoc = do
    let links =
            map
                ( \(toId, _) ->
                    FoundLink comingFrom toId (Direct assoc.associatedHow)
                )
                assoc.pointsTo
    newNodes <- forMFork assoc.pointsTo (\(toId, _) -> responseToDiscovery toId <$> fetch (GetId toId))
    pure $ links <> newNodes

responseToDiscovery :: TePapaReference -> Either ClientError TePapaThing -> Discovery
responseToDiscovery explored result =
    case result of
        Left cerr -> ErrorFetching explored cerr
        Right t -> FoundThing explored t

doQuery :: (ApiM m) => FetchReq a -> m a
doQuery (GetId tref) =
    runReq (getById tref)
doQuery (GetRelated tref) =
    let
        fetchLimit = Just 20
     in
        runReq (getRelated tref fetchLimit)
