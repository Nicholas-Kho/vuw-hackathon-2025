{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.CommonObject (
    TePapaThing (..),
    prettyPrintThing,
    parseThingWith,
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text
import TePapa.Association (Association, parseCommonOutgoingEdges)
import TePapa.ExternalId
import TePapa.ExtraFields (Extras)

data TePapaThing = TePapaThing
    { externalReference :: TePapaReference
    , title :: Text
    , associations :: [Association]
    , extras :: Maybe Extras
    }

instance FromJSON TePapaThing where
    parseJSON =
        withObject
            "a TePapa Thing"
            ( \o -> do
                eid <- o .: "id"
                label <- o .: "type"
                TePapaThing
                    <$> (pure TePapaReference{namespace = label, eid = eid})
                    <*> (o .: "title")
                    <*> (pure $ parseCommonOutgoingEdges o)
                    <*> (pure Nothing)
            )

prettyPrintThing :: TePapaThing -> String
prettyPrintThing thing = unpack thing.title

parseThingWith :: (FromJSON a) => (a -> Extras) -> Value -> Parser TePapaThing
parseThingWith constructor v = do
    commonFields <- parseJSON @TePapaThing v
    extras <- constructor <$> parseJSON v
    pure (commonFields{extras = Just extras})
