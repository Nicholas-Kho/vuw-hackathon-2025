module AiSummary.DescriptionQueue (
    WhyFetching (..),
    DescriptionHeap,
    newDescHeap,
    queueDescribe,
    popDescribe,
) where

import Cache.NodeId (NodeId)
import Control.Concurrent.STM (modifyTVar', newTVar)
import qualified Data.Heap as H
import GHC.Conc (STM, TVar, readTVar, writeTVar)

-- NOTE: The order of these constructors matters for the Ord implementation!
-- We want UserAsked to be the "smallest" and ServerBored to be the "biggest"
-- for use in min-heaps.
data WhyFetching
    = UserAsked
    | Prefetch
    | ServerBored
    deriving (Eq, Ord)

-- NOTE: We could get high contention here because pretty much every STM transaction will
-- touch the heap root and mess with it, but I don't think it will be a problem right now
type DescriptionHeap = TVar (H.MinPrioHeap WhyFetching NodeId)

newDescHeap :: STM DescriptionHeap
newDescHeap = newTVar H.empty

queueDescribe :: DescriptionHeap -> WhyFetching -> NodeId -> STM ()
queueDescribe heap prio nid = modifyTVar' heap (H.insert (prio, nid))

popDescribe :: DescriptionHeap -> STM (Maybe NodeId)
popDescribe heap = do
    oldHeap <- readTVar heap
    case H.view oldHeap of
        Nothing -> return Nothing
        Just ((_, nid), newHeap) -> do
            writeTVar heap newHeap
            return (Just nid)
