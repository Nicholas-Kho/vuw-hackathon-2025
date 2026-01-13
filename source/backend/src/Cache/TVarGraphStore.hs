module Cache.TVarGraphStore () where

import Cache.Interface
import qualified Data.Map as M
import qualified Data.Set as S
import Domain.Model
import GHC.Conc
import Servant.Client (ClientError)

data GraphContent = GraphContent
    { nodes :: M.Map NodeId Node
    , edges :: S.Set Edge
    }

type Graph = TVar GraphContent

instance GraphStore Graph STM where
    readNode = readNode'
    tryFetch = tryFetch'
    failNode = failNode'
    outgoingEdges = outgoingEdges'
    deleteNode = deleteNode'
    commitNode = commitNode'

commitNode' :: Graph -> NodeId -> NodeContent -> STM ()
commitNode' graph nid ncs = do
    conts <- readTVar graph
    let nodeMap = nodes conts
    let newNodes = M.insert nid (Ok ncs) nodeMap
    writeTVar graph (conts{nodes = newNodes})

deleteNode' :: Graph -> NodeId -> STM Node
deleteNode' graph nid = do
    edgesToRemove <- outgoingEdges' graph nid
    conts <- readTVar graph
    let newNodes = M.delete nid (nodes conts)
    let newEdges = (edges conts) `S.difference` edgesToRemove
    writeTVar graph (GraphContent newNodes newEdges)
    pure $ M.findWithDefault NotFetched nid (nodes conts)

outgoingEdges' :: Graph -> NodeId -> STM (S.Set Edge)
outgoingEdges' graph nid = do
    conts <- readTVar graph
    let edgeSet = edges conts
    pure $ S.filter (\e -> from e == nid) edgeSet

failNode' :: Graph -> NodeId -> ClientError -> STM ()
failNode' graph nid cerr = do
    conts <- readTVar graph
    let newNodes = M.insert nid (Fail cerr) (nodes conts)
    writeTVar graph (conts{nodes = newNodes})

tryFetch' :: Graph -> NodeId -> STM FetchResult
tryFetch' graph nid = do
    node <- readNode' graph nid
    case node of
        Ok nodeConts -> pure . AlreadyThere . Right $ nodeConts
        Fail cerr -> pure . AlreadyThere . Left $ cerr
        NotFetched -> pure Proceed
        Fetching -> pure WaitForResult

readNode' :: Graph -> NodeId -> STM Node
readNode' graph nid = do
    conts <- readTVar graph
    let nodeMap = nodes conts
    pure $ M.findWithDefault NotFetched nid nodeMap
