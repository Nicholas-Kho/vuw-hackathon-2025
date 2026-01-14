{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.Convert (NodeLike (..)) where

import qualified Data.Set as S
import Domain.Model
import TePapa.Decode

class NodeLike a where
    getContent :: a -> NodeContent
    getEdges :: a -> S.Set Edge

instance NodeLike Person where
    getContent p =
        NodeContent
            { title = p.com.title
            , -- TODO: support this
              thumbnailUrl = Nothing
            , description = p.givenName <> " " <> p.familyName <> "()"
            }
