{-# LANGUAGE OverloadedRecordDot #-}

module AiSummary.StartLlama where

import AiSummary.LlamaApi (checkHealth)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Servant.Client (BaseUrl (baseUrlPort), ClientEnv (baseUrl), runClientM)
import System.Process

llamaServerCommand :: FilePath
llamaServerCommand = "llama-server"

llamaServerFlags :: Int -> [String]
llamaServerFlags port =
    [ "-m"
    , "ai-models/phi.gguf"
    , "--ctx-size"
    , "512"
    , "--kv-unified"
    , "--parallel"
    , "2"
    , "--threads"
    , "6"
    , "--flash-attn"
    , "auto"
    , "--mmap"
    , "--offline"
    , "--port"
    , show port
    ]

isLlamaReady :: ClientEnv -> IO Bool
isLlamaReady cenv = do
    runClientM checkHealth cenv >>= \case
        Left _ -> return False
        Right _ -> return True

pollLlama :: ClientEnv -> Int -> Int -> IO Bool
pollLlama cenv delayMicroseconds attemptsLeft
    | attemptsLeft <= 0 = return False
    | otherwise = do
        putStrLn $
            "Checking if llama server is ready... (" <> (show attemptsLeft) <> " attempts left)"
        isGood <- isLlamaReady cenv
        if isGood
            then putStrLn "llama OK!" >> return True
            else do
                putStrLn $
                    "llama server still loading, trying again in "
                        <> (show (delayMicroseconds `div` 1000))
                        <> " millisconds."
                threadDelay delayMicroseconds
                pollLlama cenv delayMicroseconds (attemptsLeft - 1)

startLlamaWaitForReady :: ClientEnv -> IO ProcessHandle
startLlamaWaitForReady cenv = do
    let port = cenv.baseUrl.baseUrlPort
    llamaHandle <- spawnProcess llamaServerCommand (llamaServerFlags port)
    llamaStartedOk <- pollLlama cenv 2000000 10
    if llamaStartedOk
        then return llamaHandle
        else do
            terminateProcess llamaHandle
            throwIO (userError "Couldn't start llama-server after waiting for a bit.")
