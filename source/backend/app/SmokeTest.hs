module Main (main) where

import App
import Cache.TVarGraphStore (showCache)
import Control.Concurrent
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (ask), asks)
import FetchM (runFetch, runFetchConc)
import FetchStore.TePapaFetchStore
import GHC.Conc
import Servant.Client
import TePapa.Client
import TePapa.Decode (ExternalId (..), MuseumResource (..), TePapaReference (..))
import TePapa.Traverse
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
        CatRelated eid -> do
            res <- runReq $ getConceptRelated eid (Just 10)
            liftIO $ print res
            repl
        ObjectNeighs eid True -> do
            ncap <- liftIO $ getNumCapabilities
            liftIO . putStrLn $ "Running with " <> (show ncap) <> "capabilities"
            qsem <- liftIO $ newQSem 8
            fetchStore <- liftIO . atomically $ emptyStore
            env <- ask
            let fetchComp = getNeighs (TePapaReference{namespace = ObjectR, eid = ExternalId eid})
            neighActions <- liftIO $ runFetchConc qsem atomically (doQueryIO env) fetchStore fetchComp
            forM_ neighActions $ liftIO . putStrLn . prettyPrintDiscovery
            repl
        ObjectNeighs eid False -> do
            neighActions <- runFetch (doQuery) (getNeighs (TePapaReference{namespace = ObjectR, eid = ExternalId eid}))
            forM_ neighActions $ liftIO . putStrLn . prettyPrintDiscovery
            repl

doQueryIO :: AppEnv -> FetchReq a -> IO a
doQueryIO appEnv req = (doQuery req) `runAppM` appEnv

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
        "catRelated" -> pure (CatRelated idInt)
        "objectNeighs" -> pure (ObjectNeighs idInt True)
        "objectNeighsSeq" -> pure (ObjectNeighs idInt False)
        _ -> Nothing

showRes :: (Show a) => Either ClientError a -> IO ()
showRes = \case
    Left clientError -> print clientError
    Right x -> print x

data UserAction
    = ObjectById Int
    | AgentById Int
    | PlaceById Int
    | CatRelated Int
    | ObjectNeighs Int Bool
    | ShowCache
    | Quit
