module Main (main) where

import App
import Cache.TVarGraphStore (showCache)
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Servant.Client
import TePapa.Client
import Text.Read

main :: IO ()
main = do
    appEnv <- getInitialEnv
    runAppM repl appEnv

repl :: AppM ()
repl = do
    liftIO getUserAction >>= \case
        Quit -> pure ()
        ObjectById eid -> do
            res <- runReq $ getObject eid
            liftIO $ print res
            repl
        AgentById eid -> do
            res <- runReq $ getAgent eid
            liftIO $ print res
            repl
        PlaceById eid -> do
            res <- runReq $ getPlace eid
            liftIO $ print res
            repl
        ShowCache -> do
            store <- asks graph
            showCache store
            repl
            repl

getUserAction :: IO UserAction
getUserAction = do
    putStrLn "Enter action"
    userInput <- getLine
    case Prelude.words userInput of
        ["quit"] -> pure Quit
        ["show"] -> pure ShowCache
        [action, idRaw] ->
            case mkAction action idRaw of
                Nothing -> getUserAction
                Just a -> pure a
        _anyOther -> getUserAction

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
    | ShowCache
    | Quit
