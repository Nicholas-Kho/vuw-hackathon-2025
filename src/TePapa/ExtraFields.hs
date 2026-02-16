{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module TePapa.ExtraFields (
    Extras (..),
) where

import Data.Aeson
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
