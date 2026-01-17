{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Convert () where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Domain.Model
import GHC.Records (HasField)
import Servant.Client (ClientError)
import TePapa.Client
import TePapa.Decode

-- FIXME: Don't hardcode constants
maxFetchAmount :: Int
maxFetchAmount = 20

fetchRelated :: (ApiM m) => NodeId -> m (Either ClientError RelatedThings)
fetchRelated NodeId{unNodeId = (ns, x)} =
    case ns of
        ObjectN -> runReq $ getObjectRelated x (Just maxFetchAmount)
        AgentN -> runReq $ getAgentRelated x (Just maxFetchAmount)
        PlaceN -> runReq $ getPlaceRelated x (Just maxFetchAmount)

getNeighborsViaRelated :: (ApiM m) => NodeId -> m [GraphAction]
getNeighborsViaRelated nid = do
    fetchRelated nid >>= \case
        Left cerr -> pure [AddNode nid (Fail cerr)]
        Right rel -> do
            pure $
                concat
                    [ (uncurry AddNode) <$> relatedThingsToNodes rel
                    , AddEdge <$> relatedThingsToEdges nid rel
                    ]

type OutgoingCat = (T.Text, Int)

assocToOutgoingCats :: Association -> [OutgoingCat]
assocToOutgoingCats a =
    map (\r -> (a.name, unId r.eid)) $
        filter (\r -> r.namespace == ConceptR) a.pointsTo

getNeighborsViaCats :: (ApiM m) => NodeId -> m [GraphAction]
getNeighborsViaCats nid@NodeId{unNodeId = (namespace, eid)} =
    case namespace of
        ObjectN -> (fmap outgoingFromObjectResponse) <$> (runReq $ getObject eid)
        AgentN -> (fmap outgoingFromAgentResponse) <$> (runReq $ getAgent eid)
        PlaceN -> (fmap (.com.outgoing)) <$> (runReq $ getPlace eid)
        >>= \case
            Left cerr -> pure [AddNode nid (Fail cerr)]
            Right assocs -> do
                let outCats = concatMap assocToOutgoingCats assocs
                error "todo"

outgoingFromObjectResponse :: ObjectResponse -> [Association]
outgoingFromObjectResponse res = case res of
    Spc s -> s.com.outgoing
    Art a -> a.com.outgoing

outgoingFromAgentResponse :: AgentResponse -> [Association]
outgoingFromAgentResponse res = case res of
    Prs p -> p.com.outgoing
    Org o -> o.com.outgoing

relatedThingsToNodes :: RelatedThings -> [(NodeId, Node)]
relatedThingsToNodes
    RelatedThings
        { relatedSpecimens = ss
        , relatedPlaces = ps
        , relatedPeople = ple
        , relatedOrgs = os
        , relatedArtefacts = as
        } =
        concat $
            [ nodesFrom ss
            , nodesFrom ps
            , nodesFrom ple
            , nodesFrom os
            , nodesFrom as
            ]
      where
        nodesFrom :: (NodeLike a) => [a] -> [(NodeId, Node)]
        nodesFrom xs = (\x -> (mkId x, Ok . getContent $ x)) <$> xs

relatedThingsToEdges :: NodeId -> RelatedThings -> [Edge]
relatedThingsToEdges
    nid
    RelatedThings
        { relatedSpecimens = ss
        , relatedPlaces = ps
        , relatedPeople = ple
        , relatedOrgs = os
        , relatedArtefacts = as
        } =
        concat $
            [ edgesFrom nid ss
            , edgesFrom nid ps
            , edgesFrom nid ple
            , edgesFrom nid os
            , edgesFrom nid as
            ]
      where
        edgesFrom :: (HasField "com" r CommonFields, NodeLike r) => NodeId -> [r] -> [Edge]
        edgesFrom n = concatMap (`relatedToHow` n)

relatedToHow :: (HasField "com" r CommonFields, NodeLike r) => r -> NodeId -> [Edge]
relatedToHow thing target =
    map (\a -> Edge{to = target, info = a.name, from = mkId thing}) $
        filter (\a -> (nodeIdToExternal target) `elem` a.pointsTo) thing.com.outgoing

class Describable a where
    describe :: a -> T.Text

class NodeLike a where
    getContent :: a -> NodeContent
    mkId :: a -> NodeId

nodeContentFromCommon ::
    ( HasField "com" a CommonFields
    , Describable a
    ) =>
    a ->
    NodeContent
nodeContentFromCommon a =
    NodeContent
        { title = a.com.title
        , thumbnailUrl = Nothing
        , description = describe a
        }

instance Describable Person where
    describe p =
        p.verbatimBirthDate
            <> " - "
            <> (fromMaybe "present" p.verbatimDeathDate)

instance Describable Organization where
    describe o =
        "Founded "
            <> o.verbatimBirthDate
            <> (maybe "" (\dd -> ", disbanded " <> dd) o.verbatimDeathDate)

instance Describable Artefact where
    describe a = a.collectionLabel <> " - " <> a.caption

instance Describable Specimen where
    describe s = s.collectionLabel <> " - " <> s.captionFormatted

instance Describable Place where
    describe p =
        case (p.location, nations) of
            (Nothing, Nothing) -> ""
            (Nothing, Just n) -> "Located in " <> n
            (Just loc, Nothing) -> "Located in " <> (T.show loc.lat) <> ", " <> (T.show loc.lon)
            (Just loc, Just n) -> "Located in " <> (T.show loc.lat) <> ", " <> (T.show loc.lon) <> ", " <> n
      where
        nations = case T.intercalate ", " p.nation of
            "" -> Nothing
            other -> Just other

instance NodeLike Person where
    getContent = nodeContentFromCommon
    mkId x = NodeId{unNodeId = (AgentN, unId x.com.eid)}

instance NodeLike Organization where
    getContent = nodeContentFromCommon
    mkId x = NodeId{unNodeId = (AgentN, unId x.com.eid)}

instance NodeLike Artefact where
    getContent = nodeContentFromCommon
    mkId x = NodeId{unNodeId = (ObjectN, unId x.com.eid)}

instance NodeLike Specimen where
    getContent = nodeContentFromCommon
    mkId x = NodeId{unNodeId = (ObjectN, unId x.com.eid)}

instance NodeLike Place where
    getContent = nodeContentFromCommon
    mkId x = NodeId{unNodeId = (PlaceN, unId x.com.eid)}

instance NodeLike ObjectResponse where
    getContent (Art a) = getContent a
    getContent (Spc s) = getContent s
    mkId (Art a) = NodeId{unNodeId = (PlaceN, unId a.com.eid)}
    mkId (Spc s) = NodeId{unNodeId = (PlaceN, unId s.com.eid)}

instance NodeLike AgentResponse where
    getContent (Prs p) = getContent p
    getContent (Org o) = getContent o
    mkId (Prs p) = NodeId{unNodeId = (PlaceN, unId p.com.eid)}
    mkId (Org o) = NodeId{unNodeId = (PlaceN, unId o.com.eid)}
