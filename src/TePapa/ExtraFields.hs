{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.ExtraFields (
    Extras (..),
    ExtrasArtefact (..),
    ExtrasOrganisation (..),
    ExtrasPlace (..),
    ExtrasPerson (..),
    ExtrasSpecimen (..),
    ExtrasTopic (..),
    Geolocation (..),
    toExtrasMap,
) where

import Data.Aeson
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text

data Extras
    = ForPerson ExtrasPerson
    | ForOrganisation ExtrasOrganisation
    | ForArtefact ExtrasArtefact
    | ForSpecimen ExtrasSpecimen
    | ForPlace ExtrasPlace
    | ForTopic ExtrasTopic

data ExtrasSpecimen = ExtrasSpecimen
    { collectionLabel :: Text
    , captionFormatted :: Text
    }

data ExtrasArtefact = ExtrasArtefact
    { collectionLabel :: Text
    , caption :: Text
    }

data ExtrasPerson = ExtrasPerson
    { gender :: Text
    , familyName :: Text
    , givenName :: Text
    , verbatimBirthDate :: Text
    , verbatimDeathDate :: Text
    }

data ExtrasOrganisation = ExtrasOrganisation
    { verbatimBirthDate :: Text
    , verbatimDeathDate :: Text
    }

data ExtrasPlace = ExtrasPlace
    { nations :: [Text]
    , location :: Maybe Geolocation
    }

data Geolocation = Geolocation
    { lat :: Float
    , lon :: Float
    }
instance Show Geolocation where
    show gl = (Prelude.show gl.lat) <> "lat, " <> (Prelude.show gl.lon) <> "lon"

data ExtrasTopic = ExtrasTopic
    { narrative :: Text
    }

instance FromJSON Geolocation where
    parseJSON =
        withObject
            "geolocation"
            ( \o ->
                Geolocation
                    <$> o .: "lat"
                    <*> o .: "lon"
            )

class ToExtrasMap a where
    toExtrasMap :: a -> M.Map Text Text

instance ToExtrasMap ExtrasSpecimen where
    toExtrasMap e =
        M.fromList
            [ ("collectionLabel", e.collectionLabel)
            , ("captionFormatted", captionFormatted e)
            ]

instance ToExtrasMap ExtrasArtefact where
    toExtrasMap e =
        M.fromList
            [ ("collectionLabel", e.collectionLabel)
            , ("caption", caption e)
            ]

instance ToExtrasMap ExtrasPerson where
    toExtrasMap e =
        M.fromList
            [ ("gender", e.gender)
            , ("familyName", e.familyName)
            , ("givenName", e.givenName)
            , ("verbatimBirthDate", e.verbatimBirthDate)
            , ("verbatimDeathDate", e.verbatimDeathDate)
            ]

instance ToExtrasMap ExtrasOrganisation where
    toExtrasMap e =
        M.fromList
            [ ("verbatimBirthDate", e.verbatimBirthDate)
            , ("verbatimDeathDate", e.verbatimDeathDate)
            ]

instance ToExtrasMap ExtrasPlace where
    toExtrasMap e =
        M.fromList $
            catMaybes
                [ if Prelude.null (nations e) then Nothing else Just ("nations", Data.Text.intercalate ", " e.nations)
                , fmap (\loc -> ("location", Data.Text.show loc)) e.location
                ]

instance ToExtrasMap ExtrasTopic where
    toExtrasMap e = M.singleton "narrative" e.narrative

instance ToExtrasMap Extras where
    toExtrasMap (ForPerson p) = toExtrasMap p
    toExtrasMap (ForOrganisation o) = toExtrasMap o
    toExtrasMap (ForArtefact a) = toExtrasMap a
    toExtrasMap (ForSpecimen s) = toExtrasMap s
    toExtrasMap (ForPlace p) = toExtrasMap p
    toExtrasMap (ForTopic t) = toExtrasMap t
