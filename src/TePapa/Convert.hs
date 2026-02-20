{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Convert (tePapaThingToNode, edgeReasonToTxt) where

import AiSummary.CompletionTypes (toText)
import AiSummary.LlamaApi (LlamaM (llamaEnv), describeThis)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Domain.Model
import Servant.Client (runClientM)
import TePapa.CommonObject
import TePapa.Traverse

edgeReasonToTxt :: EdgeReason -> T.Text
edgeReasonToTxt r =
    case r of
        Direct relationName -> "Directly related via " <> relationName
        ShareCategory CategoryInfo{catTitle = catName, catId = _} relatedHow ->
            "Both " <> relatedHow <> " " <> catName

-- TODO: May want to call this asynchronously and handle errors better.
tePapaThingToNode :: (LlamaM m) => TePapaThing -> m NodeContent
tePapaThingToNode thing = do
    lenv <- llamaEnv
    desc <-
        liftIO $
            runClientM (describeThis thing) lenv >>= \case
                Left _ -> return "Llama failed :("
                Right r -> return r.toText
    return $
        NodeContent
            { thumbnailUrl = Nothing
            , description = desc
            , title = thing.title
            }
