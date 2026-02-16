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

tePapaThingToNode :: TePapaThing -> Maybe NodeContent
tePapaThingToNode thing =
    Just $
        NodeContent
            { title = thing.title
            , description = "TODO: Implement me!"
            , thumbnailUrl = Nothing
            }
