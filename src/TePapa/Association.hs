module TePapa.Association (
    Association (..),
    parseCommonOutgoingEdges,
) where

import Data.Aeson
import Data.Aeson.KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Text
import qualified Data.Vector as V
import GHC.Generics
import TePapa.ExternalId (TePapaReference, parseReferenceyObject)

data Association = Association
    { name :: Text
    , pointsTo :: [(TePapaReference, Text)]
    }
    deriving (Show, Generic)

associationParserHelper :: Key -> Value -> Parser Association
associationParserHelper k =
    withArray
        "association"
        ( \a ->
            Association
                <$> (pure . Data.Text.show $ k)
                <*> (Prelude.traverse parseReferenceyObject (V.toList a))
        )

parseCommonOutgoingEdges :: Object -> [Association]
parseCommonOutgoingEdges = elems . mapMaybeWithKey (\k v -> parseMaybe (associationParserHelper k) v)
