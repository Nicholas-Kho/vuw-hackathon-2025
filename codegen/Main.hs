{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Api.Backend
import Cache.NodeId (NodeId)
import Domain.Model (Node, NodeContent)
import Servant.Elm
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    outputDir <- case args of
        [p] -> pure p
        _other -> die "Please call this program with an output directory as the one and only argument."
    generateElmModule
        ["Generated", "BackendApi"]
        ""
        outputDir
        [ DefineElm (Proxy @NodeId)
        , DefineElm (Proxy @NodeContent)
        , DefineElm (Proxy @Node)
        , DefineElm (Proxy @InitialGameState)
        , DefineElm (Proxy @UnverifiedNodeId)
        , DefineElm (Proxy @ExpandParams)
        ]
        (Proxy @ApiRoutes)
