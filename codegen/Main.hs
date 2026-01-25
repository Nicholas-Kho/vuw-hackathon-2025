{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Api.Backend
import Cache.NodeId (NodeId)
import Domain.Model (Node, NodeContent)
import Servant.Elm

main :: IO ()
main = do
    outputDir <- pure "frontend/src/Generated"
    pure ()
    generateElmModule
        ["Generated"]
        "BackendApi"
        outputDir
        [ DefineElm (Proxy @NodeId)
        , DefineElm (Proxy @NodeContent)
        , DefineElm (Proxy @Node)
        , DefineElm (Proxy @InitialGameState)
        , DefineElm (Proxy @UnverifiedNodeId)
        , DefineElm (Proxy @ExpandParams)
        ]
        (Proxy @ApiRoutes)
