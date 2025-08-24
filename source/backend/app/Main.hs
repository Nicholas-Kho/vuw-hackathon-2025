{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Configuration.Dotenv as DE
import Config (Config (..), Stage(..), mkConfig, pickLogger)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Sql (runSqlPool)
import Models (addReferenceCheckConstraint, doMigrations)
import Network.Wai.Handler.Warp (Port)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  DE.loadFile DE.defaultConfig
  port <- getPort
  stage <- decideStage
  config <- mkConfig stage
  let loggerMiddleware = pickLogger stage
      pool = getPool config
  runSqlPool (doMigrations >> addReferenceCheckConstraint) pool

getPort :: IO Port
getPort = do
  rawPort <- lookupEnv "PORT"
  return $ (rawPort >>= readMaybe @Int >>= ensure (>2048)) `withDefault` 8080

decideStage :: IO Stage
decideStage = pure Development

ensure :: MonadFail m => (a -> Bool) -> a -> m a
ensure predicate val = if predicate val then pure val else fail ""

withDefault :: Maybe a -> a -> a
withDefault Nothing fallback = fallback
withDefault (Just x) _ = x
