module AiSummary.PostProcess where

import qualified Data.Set as S
import Data.Text (Text)
import TePapa.ExternalId (TePapaReference)

data LinkInfo = LinkInfo
    { text :: Text
    , goesTo :: TePapaReference
    }

infoFromTuple :: (TePapaReference, Text) -> LinkInfo
infoFromTuple = uncurry (flip LinkInfo)

data DescriptionBit
    = Txt Text
    | Link LinkInfo

newtype Description = FromList {toList :: [DescriptionBit]}

data PostProcessResult = PostProcessResult
    { description :: Description
    , linksMissing :: S.Set LinkInfo
    }

findLinks :: Text -> S.Set LinkInfo -> PostProcessResult
findLinks haystack needles = error "todo"
