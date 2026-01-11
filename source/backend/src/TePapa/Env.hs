module TePapa.Env (getApiKey) where

import qualified Configuration.Dotenv as Dotenv
import Data.Text
import System.Environment

expectKey :: Maybe String -> String
expectKey (Just key) = key
expectKey Nothing =
    error
        "The environment variable 'API_KEY' doesn't exist. Make sure you have a .env file with this variable!"

getApiKey :: IO Text
getApiKey = do
    Dotenv.loadFile Dotenv.defaultConfig
    pack . expectKey <$> lookupEnv "API_KEY"
