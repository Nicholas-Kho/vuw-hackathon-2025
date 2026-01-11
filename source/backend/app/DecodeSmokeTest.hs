module Main (main, reqObject) where

import Network.HTTP.Client.TLS
import Servant.Client
import TePapa.Client
import TePapa.Env

main :: IO ()
main = pure ()

testClientEnv :: IO ClientEnv
testClientEnv = do
    manager <- newTlsManager
    pure $ mkClientEnv manager collectionsURL

reqObject :: Int -> IO ()
reqObject eid = do
    apiKey <- getApiKey
    clientEnv <- testClientEnv
    let clientm = getObject apiKey eid
    result <- runClientM clientm clientEnv
    case result of
        Left clientError -> print clientError
        Right objectResponse -> print objectResponse
