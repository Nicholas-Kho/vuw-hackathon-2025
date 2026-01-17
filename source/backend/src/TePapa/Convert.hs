{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Convert () where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Domain.Model
import GHC.Records (HasField)
import TePapa.Decode

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
