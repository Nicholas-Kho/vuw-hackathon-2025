{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Convert (
    NodeLike (..),
    fetchReference,
) where

import Api.TePapa (ApiKey)
import Data.Maybe
import qualified Data.Set as S
import Data.Text
import qualified Data.Text as T
import Domain.Model
import GHC.Records
import Servant.Client (ClientM)
import TePapa.Client
import TePapa.Decode

fetchReference :: TePapaReference -> ApiKey -> ClientM GraphFragment
fetchReference ref key =
    case namespace ref of
        ObjectR -> toFragment <$> getObject (unId ref.eid) key
        AgentR -> toFragment <$> getAgent (unId ref.eid) key
        PlaceR -> toFragment <$> getPlace (unId ref.eid) key
        notImplemented -> error $ (Prelude.show notImplemented) <> " is not implemented"

outgoingEdgeConvert :: TePapa.Decode.Edge -> S.Set PartEdge
outgoingEdgeConvert
    TePapa.Decode.Edge
        { direction = d
        , association = Association{name = n, pointsTo = rs}
        } =
        case d of
            TePapa.Decode.Incoming -> S.empty
            TePapa.Decode.Outgoing -> S.map (\r -> (r, n)) (S.fromList rs)

class Describable a where
    describe :: a -> Text

class NodeLike a where
    getContent :: a -> NodeContent
    getOutEdges :: a -> S.Set PartEdge
    getInEdges :: a -> S.Set PartEdge
    toFragment :: a -> GraphFragment
    toFragment a =
        GraphFragment
            { content = getContent a
            , outEdges = getOutEdges a
            , inEdges = getInEdges a
            }

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

outEdgesFromCommon :: (HasField "com" a CommonFields) => a -> S.Set PartEdge
outEdgesFromCommon a = Prelude.foldl' S.union S.empty (outgoingEdgeConvert <$> a.com.outgoing)

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
    getOutEdges = outEdgesFromCommon
    getInEdges _ = S.empty

instance NodeLike Organization where
    getContent = nodeContentFromCommon
    getOutEdges = outEdgesFromCommon
    getInEdges _ = S.empty

instance NodeLike Artefact where
    getContent = nodeContentFromCommon
    getOutEdges = outEdgesFromCommon
    getInEdges _ = S.empty

instance NodeLike Specimen where
    getContent = nodeContentFromCommon
    getOutEdges = outEdgesFromCommon
    getInEdges _ = S.empty

instance NodeLike Place where
    getContent = nodeContentFromCommon
    getOutEdges = outEdgesFromCommon
    getInEdges _ = S.empty

instance NodeLike ObjectResponse where
    getContent (Art a) = getContent a
    getContent (Spc s) = getContent s
    getOutEdges (Art a) = getOutEdges a
    getOutEdges (Spc s) = getOutEdges s
    getInEdges _ = S.empty

instance NodeLike AgentResponse where
    getContent (Prs p) = getContent p
    getContent (Org o) = getContent o
    getOutEdges (Prs p) = getOutEdges p
    getOutEdges (Org o) = getOutEdges o
    getInEdges _ = S.empty
