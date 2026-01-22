{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Convert () where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Domain.Model
import GHC.Records (HasField)
import TePapa.CommonObject
import TePapa.Decode
import TePapa.Traverse

edgeReasonToTxt :: EdgeReason -> T.Text
edgeReasonToTxt r =
    case r of
        Direct relationName -> "Directly related via " <> relationName
        ShareCategory CategoryInfo{catTitle = catName, catId = _} relatedHow ->
            "Both " <> relatedHow <> " " <> catName

class Describable a where
    describe :: a -> T.Text

class NodeLike a where
    getContent :: a -> NodeContent

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

instance NodeLike Organization where
    getContent = nodeContentFromCommon

instance NodeLike Artefact where
    getContent = nodeContentFromCommon

instance NodeLike Specimen where
    getContent = nodeContentFromCommon

instance NodeLike Place where
    getContent = nodeContentFromCommon

instance NodeLike ObjectResponse where
    getContent (Art a) = getContent a
    getContent (Spc s) = getContent s

instance NodeLike AgentResponse where
    getContent (Prs p) = getContent p
    getContent (Org o) = getContent o

tePapaThingToNode :: TePapaThing -> Maybe NodeContent
tePapaThingToNode thing =
    case thing of
        APerson p -> Just $ getContent p
        AnOrg o -> Just $ getContent o
        AnArtefact a -> Just $ getContent a
        ASpecimen s -> Just $ getContent s
        APlace p -> Just $ getContent p
        ATopic _ -> Nothing
        ACategory _ -> Nothing
