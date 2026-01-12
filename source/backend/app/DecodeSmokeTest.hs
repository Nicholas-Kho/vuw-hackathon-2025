module Main (main) where

import Data.Text
import Network.HTTP.Client.TLS
import Servant.Client
import TePapa.Client
import TePapa.Env
import Text.Read

main :: IO ()
main = do
    clientEnv <- testClientEnv
    apiKey <- getApiKey
    repl (clientEnv, apiKey)

repl :: (ClientEnv, Text) -> IO ()
repl resources@(clientEnv, apiKey) = do
    userInput <- getUserAction
    case userInput of
        Quit -> pure ()
        ObjectById eid ->
            runClientM (getObject apiKey eid) clientEnv >>= showRes >> repl resources
        AgentById eid ->
            runClientM (getAgent apiKey eid) clientEnv >>= showRes >> repl resources
        PlaceById eid ->
            runClientM (getPlace apiKey eid) clientEnv >>= showRes >> repl resources

getUserAction :: IO UserAction
getUserAction = do
    putStrLn "Enter action"
    userInput <- getLine
    case Prelude.words userInput of
        ["quit"] -> pure Quit
        [action, idRaw] ->
            case mkAction action idRaw of
                Nothing -> getUserAction
                Just a -> pure a
        _ -> getUserAction

mkAction :: String -> String -> Maybe UserAction
mkAction action idRaw = do
    idInt <- readMaybe @Int idRaw
    case action of
        "object" -> pure (ObjectById idInt)
        "agent" -> pure (AgentById idInt)
        "place" -> pure (PlaceById idInt)
        _ -> Nothing

showRes :: (Show a) => Either ClientError a -> IO ()
showRes = \case
    Left clientError -> print clientError
    Right x -> print x

data UserAction
    = ObjectById Int
    | AgentById Int
    | PlaceById Int
    | Quit

testClientEnv :: IO ClientEnv
testClientEnv = do
    manager <- newTlsManager
    pure $ mkClientEnv manager collectionsURL
