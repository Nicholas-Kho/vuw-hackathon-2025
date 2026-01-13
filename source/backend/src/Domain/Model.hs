module Domain.Model (
    Edge (..),
    Node (..),
    NodeContent (..),
    NodeId,
)
where

import Data.Text
import Servant.Client (ClientError)
import TePapa.Decode (TePapaReference)

type NodeId = TePapaReference

data NodeContent = NodeContent
    { title :: Text
    , description :: Text
    , thumbnailUrl :: Maybe Text
    }

data Node
    = NotFetched
    | Fetching
    | Ok NodeContent
    | -- Note: Might want to change to something else later so
      -- we're not coupled to Servant
      Fail ClientError

data Edge = Edge
    { from :: NodeId
    , to :: NodeId
    , info :: Text
    }
    deriving (Eq, Ord, Show)
