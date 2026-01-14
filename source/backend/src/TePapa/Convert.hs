{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Convert (NodeLike (..)) where

import Data.Maybe
import qualified Data.Set as S
import Data.Text
import Domain.Model
import GHC.Records
import TePapa.Decode

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
    outEdges :: a -> S.Set PartEdge
    inEdges :: a -> S.Set PartEdge

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

instance NodeLike Person where
    getContent = nodeContentFromCommon
    outEdges = outEdgesFromCommon
    inEdges _ = S.empty

instance NodeLike Organization where
    getContent = nodeContentFromCommon
    outEdges = outEdgesFromCommon
    inEdges _ = S.empty

instance NodeLike Artefact where
    getContent = nodeContentFromCommon
    outEdges = outEdgesFromCommon
    inEdges _ = S.empty

instance NodeLike Specimen where
    getContent = nodeContentFromCommon
    outEdges = outEdgesFromCommon
    inEdges _ = S.empty
