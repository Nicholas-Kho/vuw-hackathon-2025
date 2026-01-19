module TePapa.Env (
    getApiKey,
    getPort,
    getSemaphore,
    loadDotEnv,
) where

import qualified Configuration.Dotenv as Dotenv
import Control.Concurrent (QSem, newQSem)
import Data.Text
import System.Environment
import Text.Read (readMaybe)

expectKey :: Maybe String -> String
expectKey (Just key) = key
expectKey Nothing =
    error
        "The environment variable 'API_KEY' doesn't exist. Make sure you have a .env file with this variable!"

getApiKey :: IO Text
getApiKey =
    pack . expectKey <$> lookupEnv "API_KEY"

loadDotEnv :: IO ()
loadDotEnv = Dotenv.loadFile Dotenv.defaultConfig

getSemaphore :: IO QSem
getSemaphore =
    lookupEnv "MAX_CONCURRENT_HTTP" >>= \case
        Nothing -> newQSem 8
        Just x ->
            case readMaybe @Int x of
                Nothing -> newQSem 8
                Just k -> newQSem k

getPort :: IO Int
getPort = do
    lookupEnv "PORT" >>= \case
        Nothing -> pure 8080
        Just rawPort ->
            case readMaybe @Int rawPort of
                Nothing -> pure 8080
                Just k ->
                    if k <= 2048
                        then error "Please pick a port number above 2048."
                        else pure k
