{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Convert (
    GraphAction (..),
    discoveryToAction,
    graphActionPrettyPrint,
) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Domain.Model
import GHC.Generics
import GHC.Records (HasField)
import TePapa.CommonObject
import TePapa.Decode
import TePapa.Traverse

data GraphAction
    = AddNode NodeId Node
    | AddEdge Edge
    deriving (Show, Generic, ToJSON)

graphActionPrettyPrint :: GraphAction -> IO ()
graphActionPrettyPrint (AddNode nid n) =
    putStrLn $ prettyPrintNodeId nid <> " ::= " <> prettyPrintNode n
graphActionPrettyPrint (AddEdge Edge{from = fid, to = tid, info = inf}) =
    putStrLn $
        prettyPrintNodeId fid
            <> " -> "
            <> prettyPrintNodeId tid
            <> " because "
            <> T.unpack inf

discoveryToAction :: Discovery -> [GraphAction]
discoveryToAction d =
    case d of
        FoundThing _ tthing ->
            maybe [] (\(nid, ncon) -> [AddNode nid ncon]) (tePapaThingToNode tthing)
        ErrorFetching _ _ -> []
        FoundLink fromTref toTref why ->
            case Edge
                <$> (trefToNodeId fromTref)
                <*> (trefToNodeId toTref)
                <*> (pure $ edgeReasonToTxt why) of
                Nothing -> []
                Just e -> [AddEdge e, AddEdge (flipEdge e)]

flipEdge :: Edge -> Edge
flipEdge Edge{to = tid, info = inf, from = fid} =
    Edge{to = fid, info = inf, from = tid}

edgeReasonToTxt :: EdgeReason -> T.Text
edgeReasonToTxt r =
    case r of
        Direct relationName -> "Directly related via " <> relationName
        ShareCategory CategoryInfo{catTitle = catName, catId = _} relatedHow ->
            "Both " <> relatedHow <> " " <> catName

class Describable a where
    describe :: a -> T.Text

class NodeLike a where
    getContent :: a -> Node
    mkId :: a -> NodeId

nodeContentFromCommon ::
    ( HasField "com" a CommonFields
    , Describable a
    ) =>
    a ->
    Node
nodeContentFromCommon a =
    Node
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

tePapaThingToNode :: TePapaThing -> Maybe (NodeId, Node)
tePapaThingToNode thing =
    case thing of
        APerson p -> Just (mkId p, getContent p)
        AnOrg o -> Just (mkId o, getContent o)
        AnArtefact a -> Just (mkId a, getContent a)
        ASpecimen s -> Just (mkId s, getContent s)
        APlace p -> Just (mkId p, getContent p)
        ATopic _ -> Nothing
        ACategory _ -> Nothing

trefToNodeId :: TePapaReference -> Maybe NodeId
trefToNodeId tref =
    case tref.namespace of
        ObjectR -> Just $ NodeId{unNodeId = (ObjectN, unId tref.eid)}
        AgentR -> Just $ NodeId{unNodeId = (AgentN, unId tref.eid)}
        PlaceR -> Just $ NodeId{unNodeId = (PlaceN, unId tref.eid)}
        ConceptR -> Nothing
        TopicR -> Nothing
