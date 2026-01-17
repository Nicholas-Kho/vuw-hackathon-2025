{-# LANGUAGE OverloadedRecordDot #-}

module TePapa.Traverse (
    CategoryInfo (..),
    Discovery (..),
    EdgeReason (..),
    fetchFromAPI,
    getDirectNeighs,
    getNeighs,
    getNeighborsViaCats,
) where

import Control.Monad
import qualified Data.Text as T
import FetchM (FetchM, fetch, runFetch)
import Servant.Client (ClientError)
import TePapa.Client (ApiM (..), getAgent, getAgentRelated, getCategory, getConceptRelated, getObject, getObjectRelated, getPlace, getPlaceRelated, getTopic, getTopicRelated)
import TePapa.CommonObject
import TePapa.Decode (Association (..), CommonFields (..), ExternalId (..), MuseumResource (..), RelatedThings, TePapaReference (..))

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

data FetchReq a where
    GetId :: TePapaReference -> FetchReq (Either ClientError TePapaThing)
    GetRelated :: TePapaReference -> FetchReq (Either ClientError RelatedThings)

type TFetch a = FetchM FetchReq a

getNeighs :: TePapaReference -> TFetch [Discovery]
getNeighs ofId = (<>) <$> getDirectNeighs ofId <*> getNeighborsViaCats ofId

getDirectNeighs ::
    TePapaReference ->
    TFetch [Discovery]
getDirectNeighs ofId = do
    outgoing <- fmap (outgoing . getCommon) <$> fetch (GetId ofId)
    case outgoing of
        Left cerr -> pure [ErrorFetching ofId cerr]
        Right assocs -> concat <$> forM assocs (directNeighsFromAssoc ofId)

directNeighsFromAssoc ::
    TePapaReference ->
    Association ->
    TFetch [Discovery]
directNeighsFromAssoc comingFrom assoc = do
    let links =
            map
                ( \(toId, _) ->
                    FoundLink comingFrom toId (Direct assoc.name)
                )
                assoc.pointsTo
    newNodes <- forM assoc.pointsTo (\(toId, _) -> responseToDiscovery toId <$> fetch (GetId toId))
    pure $ links <> newNodes

responseToDiscovery :: TePapaReference -> Either ClientError TePapaThing -> Discovery
responseToDiscovery explored result =
    case result of
        Left cerr -> ErrorFetching explored cerr
        Right t -> FoundThing explored t

getNeighborsViaCats :: TePapaReference -> TFetch [Discovery]
getNeighborsViaCats fromId =
    fetch (GetId fromId) >>= \case
        Left cerr -> pure [ErrorFetching fromId cerr]
        Right thing -> do
            let assocsPointingToCats = map (\a -> a{pointsTo = filter (\(r, _) -> r.namespace == ConceptR) a.pointsTo}) (getCommon thing).outgoing
            let relatedCats = concatMap (\a -> map (\(r, t) -> (a.name, CategoryInfo{catTitle = t, catId = unId r.eid})) a.pointsTo) assocsPointingToCats
            concat <$> (forM relatedCats $ \(relatedHow, catInfo) -> neighsViaRelatedCat fromId relatedHow catInfo)

neighsViaRelatedCat :: TePapaReference -> T.Text -> CategoryInfo -> TFetch [Discovery]
neighsViaRelatedCat comingFrom comingFromWhy catInfo = do
    let catRef = TePapaReference{namespace = ConceptR, eid = ExternalId catInfo.catId}
    fetch (GetRelated catRef) >>= \case
        Left cerr -> pure [ErrorFetching catRef cerr]
        Right related -> do
            let things = filter (\t -> getId t /= comingFrom) $ fromRelated related
            pure
                . concatMap
                    ( \t ->
                        map
                            ( \_ ->
                                FoundLink comingFrom (getId t) (ShareCategory catInfo comingFromWhy)
                            )
                            . filter (\a -> a.name == comingFromWhy && catRef `elem` (fst <$> a.pointsTo))
                            $ (getCommon t).outgoing
                    )
                $ things

-- TODO: Later integrate some cache for request deduplication here
-- Fetching policy should also come in here
fetchFromAPI :: (ApiM m) => TFetch a -> m a
fetchFromAPI = runFetch doQuery

doQuery :: (ApiM m) => FetchReq a -> m a
doQuery (GetId tref) =
    case tref.namespace of
        ObjectR -> (fmap fromObjectResponse) <$> (runReq . getObject . unId $ tref.eid)
        AgentR -> (fmap fromAgentResponse) <$> (runReq . getAgent . unId $ tref.eid)
        PlaceR -> (fmap fromPlace) <$> (runReq . getPlace . unId $ tref.eid)
        ConceptR -> (fmap fromCategory) <$> (runReq . getCategory . unId $ tref.eid)
        TopicR -> (fmap fromTopic) <$> (runReq . getTopic . unId $ tref.eid)
doQuery (GetRelated tref) =
    case tref.namespace of
        ObjectR -> runReq $ getObjectRelated (unId tref.eid) (Just 20)
        AgentR -> runReq $ getAgentRelated (unId tref.eid) (Just 20)
        PlaceR -> runReq $ getPlaceRelated (unId tref.eid) (Just 20)
        ConceptR -> runReq $ getConceptRelated (unId tref.eid) (Just 20)
        TopicR -> runReq $ getTopicRelated (unId tref.eid) (Just 20)
