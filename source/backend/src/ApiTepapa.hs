{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{-
This module contains a spec for a subset of the Te Papa Collections Api, not
the one that our backend exposes.
See https://data.tepapa.govt.nz/docs/ and
https://github.com/te-papa/collections-api/wiki
-}
module ApiTepapa () where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Aeson.TH
import Data.Aeson.Types (Parser, Value (..))
import Data.Text (Text)
import Servant

type ApiTepapa =
    "agent" :> Capture "id" Int :> Get '[JSON] Value
