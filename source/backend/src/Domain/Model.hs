module Domain.Model (
    Edge (..),
    GraphData (..),
    Graph,
    Node (..),
    NodeContent (..),
)
where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text
import GHC.Conc (TVar)
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

data GraphData = GraphData
    { nodes :: M.Map NodeId Node
    , edges :: S.Set Edge
    }

type Graph = TVar GraphData
