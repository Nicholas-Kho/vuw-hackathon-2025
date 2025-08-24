{-# LANGUAGE OverloadedStrings #-}
module Config
  ( Config(..)
  , Stage(..)
  )
where

import Control.Monad.Logger (MonadLoggerIO, runNoLoggingT, runStdoutLoggingT)
import Conduit (MonadUnliftIO)
import Database.Persist.SqlBackend.Internal (SqlBackend)
import Database.Persist.Sqlite (createSqlitePool)
import Data.Pool (Pool)
import Data.Text (Text)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai (Middleware)


type ConnectionPool = Pool SqlBackend

data Stage
  = Test
  | Development
  | Production
  deriving (Eq, Show, Read)

data Config = MkConfig
  { getPool :: ConnectionPool
  , getStage :: Stage
  }

setLogger :: Stage -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

getConnPool :: Stage -> IO ConnectionPool
getConnPool Test = runNoLoggingT $ createSqlitePool (connStr Test) (poolSize Test)
getConnPool e = runStdoutLoggingT $ createSqlitePool (connStr e) (poolSize e)

connStr :: Stage -> Text
connStr _ = ""

poolSize :: Stage -> Int
poolSize Test = 2
poolSize Development = 4
poolSize Production = 8
