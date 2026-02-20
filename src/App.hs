{-# LANGUAGE OverloadedRecordDot #-}

module App where

import AiSummary.CompletionTypes (toText)
import AiSummary.DescriptionQueue (DescribeJob (itemInfo), popDescribe, updateId)
import AiSummary.LlamaApi (describeThis, llamaEnv)
import AppM (AppEnv (descriptionQueue), graph, runAppM, setupApp)
import Cache.Interface (updateContents)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (atomically)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Text as T
import Domain.Model (NodeContent (..), NodeDescription (..))
import Domain.Server (app, devCors)
import Env (getStaticPath, getUseCors, portToServeOn)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors)
import Servant.Client (runClientM)

startServer :: AppEnv -> IO ()
startServer appEnv = do
    port <- portToServeOn
    staticPath <- getStaticPath
    useCors <- getUseCors
    let corsMiddleware = if useCors then cors (const $ Just devCors) else id
    putStrLn $ "Listening on port " <> (show port)
    run port $ corsMiddleware (app appEnv staticPath)

-- TODO: Move this somewhere else
startDescriptionWorker :: AppEnv -> IO ()
-- do some stuff here...
startDescriptionWorker =
    runAppM
        ( forever $ do
            queue <- asks descriptionQueue
            store <- asks graph
            lenv <- llamaEnv
            job <- liftIO . atomically $ popDescribe queue
            liftIO . atomically $ updateContents store job.updateId (\c -> c{description = Loading})
            desc <-
                liftIO (runClientM (describeThis job.itemInfo) lenv) >>= \case
                    Left cerr -> return . Fail . T.show $ cerr
                    Right result -> return . Ok $ result.toText
            liftIO . atomically $ updateContents store job.updateId (\c -> c{description = desc})
        )

go :: IO ()
go = do
    appEnv <- setupApp
    concurrently_
        (startServer appEnv)
        (startDescriptionWorker appEnv)
