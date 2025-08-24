{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Configuration.Dotenv as DE
import Config (Stage(..))
import Models (migrateAll)
import Network.Wai.Handler.Warp (Port)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  DE.loadFile DE.defaultConfig
  port <- getPort
  stage <- getStage
  pure ()

getPort :: IO Port
getPort = do
  rawPort <- lookupEnv "PORT"
  return $ (rawPort >>= readMaybe @Int >>= ensure (>2048)) `withDefault` 8080

getStage :: IO Stage
getStage = pure Development

ensure :: MonadFail m => (a -> Bool) -> a -> m a
ensure predicate val = if predicate val then pure val else fail ""

withDefault :: Maybe a -> a -> a
withDefault Nothing fallback = fallback
withDefault (Just x) _ = x
