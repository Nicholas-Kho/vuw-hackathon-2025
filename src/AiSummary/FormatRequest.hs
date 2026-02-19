{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module AiSummary.FormatRequest (
    Prompt (..),
    mkDescribeParams,
) where

import AiSummary.CompletionTypes (CompletionParams (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, pack)
import TePapa.Association (Association (..))
import TePapa.CommonObject (TePapaThing (..))
import TePapa.ExtraFields (toExtrasMap)

generalInstructions :: Text
generalInstructions =
    pack $
        unlines
            [ "You are a catalogue description generator."
            , "Use only the provided facts."
            , "Do not infer missing information."
            , "Do not add historical or cultural context."
            , "Do not explain anything."
            , "Write in short, simple declarative sentences."
            , "Output only the description text."
            ]

data Prompt = Prompt
    { systemText :: Text
    , userText :: Text
    }

showPrompt :: Prompt -> Text
showPrompt
    ( Prompt
            { systemText = sysText
            , userText = uText
            }
        ) =
        let
            systemTag = "<|system|>"
            userTag = "<|user|>"
            assistantTag = "<|assistant|>"
         in
            systemTag <> sysText <> userTag <> uText <> assistantTag

data ThingInfo = ThingInfo
    { title :: Text
    , references :: M.Map Text [Text]
    , properties :: M.Map Text Text
    }

mkPrompt :: ThingInfo -> Prompt
mkPrompt
    ( ThingInfo
            { title = title
            , references = references
            , properties = properties
            }
        ) =
        let
            mentionThese =
                "\nMention the following properties exactly as written: "
                    <> intercalate ", " (M.keys references)
                    <> "."
            userText =
                "Title: "
                    <> title
                    <> "\n"
                    <> intercalate "\n" (fmap (\(k, v) -> k <> ": " <> v) $ M.assocs properties)
         in
            Prompt
                { systemText = generalInstructions <> mentionThese
                , userText = userText
                }

makeAssocsMap :: [Association] -> M.Map Text [Text]
makeAssocsMap = foldl' (\m a -> M.insertWith (<>) a.associatedHow (snd <$> a.pointsTo) m) M.empty

getInfo :: TePapaThing -> ThingInfo
getInfo
    ( TePapaThing
            { title = thingTitle
            , extras = extras
            , externalReference = _externalReference
            , associations = associations
            }
        ) =
        let
         in ThingInfo
                { title = thingTitle
                , references = makeAssocsMap associations
                , properties = fromMaybe M.empty (toExtrasMap <$> extras)
                }

mkDescribeParams :: TePapaThing -> CompletionParams
mkDescribeParams thing =
    CompletionParams
        { topP = 0.9
        , temperature = 0.1
        , repeatPenalty = 1.1
        , maxTokens = 400
        , prompt = showPrompt . mkPrompt . getInfo $ thing
        }
