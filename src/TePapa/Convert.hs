{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Convert (tePapaThingToNode, edgeReasonToTxt) where

import qualified Data.Text as T
import Domain.Model
import TePapa.CommonObject
import TePapa.Traverse

edgeReasonToTxt :: EdgeReason -> T.Text
edgeReasonToTxt r =
    case r of
        Direct relationName -> "Directly related via " <> relationName
        ShareCategory CategoryInfo{catTitle = catName, catId = _} relatedHow ->
            "Both " <> relatedHow <> " " <> catName

-- TODO: May want to call this asynchronously and handle errors better.
tePapaThingToNode :: TePapaThing -> NodeContent
tePapaThingToNode thing =
    NodeContent
        { thumbnailUrl = Nothing
        , description = NotAsked
        , title = thing.title
        }
