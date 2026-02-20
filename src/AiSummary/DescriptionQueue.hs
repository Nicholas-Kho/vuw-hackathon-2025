module AiSummary.DescriptionQueue (
    WhyFetching (..),
    DescribeJob (..),
    DescriptionHeap,
    newDescHeap,
    queueDescribe,
    popDescribe,
) where

import AiSummary.FormatRequest (ThingInfo)
import Cache.NodeId (NodeId)
import Control.Concurrent.STM (modifyTVar', newTVar)
import qualified Data.Heap as H
import GHC.Conc (STM, TVar, readTVar, retry, writeTVar)

data DescribeJob = DescribeJob
    { updateId :: NodeId
    , itemInfo :: ThingInfo
    }

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
type DescriptionHeap = TVar (H.MinPrioHeap WhyFetching DescribeJob)

newDescHeap :: STM DescriptionHeap
newDescHeap = newTVar H.empty

queueDescribe :: DescriptionHeap -> WhyFetching -> DescribeJob -> STM ()
queueDescribe heap prio job = modifyTVar' heap (H.insert (prio, job))

popDescribe :: DescriptionHeap -> STM DescribeJob
popDescribe heap = do
    oldHeap <- readTVar heap
    case H.view oldHeap of
        Nothing -> retry
        Just ((_, job), newHeap) -> do
            writeTVar heap newHeap
            return job
