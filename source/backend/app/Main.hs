{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Config (Config (..), Stage (..), mkConfig, pickLogger)
import Configuration.Dotenv as DE
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text, pack)
import Database.Persist.Sql (runSqlPool)
import Models (doMigrations)
import Network.Wai.Handler.Warp (Port, defaultSettings)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import ApiTepapa (agentById)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant
import Servant.Client (BaseUrl (..), Scheme (..), mkClientEnv, runClientM)

main :: IO ()
main = do
    DE.loadFile DE.defaultConfig
    port <- getPort
    stage <- decideStage
    config <- mkConfig stage
    key <- expectKey <$> lookupEnv "API_KEY"
    let loggerMiddleware = pickLogger stage
        pool = getPool config
    runStdoutLoggingT $ runSqlPool doMigrations pool
    apiTest key

apiTest :: String -> IO ()
apiTest key = do
    manager <- newManager tlsManagerSettings
    res <- runClientM (agentById (Just (pack key)) 11327) (mkClientEnv manager baseUrl)
    print res

baseUrl :: BaseUrl
baseUrl =
    BaseUrl
        { baseUrlScheme = Https
        , baseUrlHost = "data.tepapa.govt.nz"
        , baseUrlPort = 443
        , baseUrlPath = "collection/"
        }

getPort :: IO Port
getPort = do
    rawPort <- lookupEnv "PORT"
    return $ (rawPort >>= readMaybe @Int >>= ensure (> 2048)) `withDefault` 8080

decideStage :: IO Stage
decideStage = pure Development

ensure :: (MonadFail m) => (a -> Bool) -> a -> m a
ensure predicate val = if predicate val then pure val else fail ""

withDefault :: Maybe a -> a -> a
withDefault Nothing fallback = fallback
withDefault (Just x) _ = x

expectKey :: Maybe String -> String
expectKey (Just k) = k
expectKey Nothing = error "You need an API key to be able to call the Te Papa API. Make sure there is an API_KEY=... variable in your .env file."
