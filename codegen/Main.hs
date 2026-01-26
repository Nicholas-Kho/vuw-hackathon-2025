{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Api.Backend
import Cache.NodeId (NodeId)
import qualified Data.Text as T
import Domain.Model (EdgeInfo, NodeContent, NodeElm)
import Options.Applicative
import Servant.Elm

data UserInput = UserInput
    { outputDir :: FilePath
    , apiAuthority :: ApiAuthority
    }

data ApiAuthority
    = Localhost Int
    | Host T.Text

main :: IO ()
main = do
    args <- execParser userInputPI
    generateElmModuleWith
        (mkElmOpts (apiAuthority args))
        ["Generated", "BackendApi"]
        defElmImports
        (outputDir args)
        [ DefineElm (Proxy @NodeId)
        , DefineElm (Proxy @NodeContent)
        , DefineElm (Proxy @NodeElm)
        , DefineElm (Proxy @InitialGameState)
        , DefineElm (Proxy @UnverifiedNodeId)
        , DefineElm (Proxy @ExpandParams)
        , DefineElm (Proxy @Subgraph)
        , DefineElm (Proxy @EdgeInfo)
        ]
        (Proxy @ApiRoutes)

userInputPI :: ParserInfo UserInput
userInputPI = info userInputP mempty

userInputP :: Parser UserInput
userInputP =
    UserInput
        <$> strArgument
            ( help "directory to write API bindings"
                <> metavar "write-to-dir"
            )
        <*> apiHostP

apiHostP :: Parser ApiAuthority
apiHostP = localhostP <|> domP

domP :: Parser ApiAuthority
domP =
    Host
        <$> strOption
            ( long "api-auth"
                <> help "Authority hosting the API without the leading slash"
            )
localhostP :: Parser ApiAuthority
localhostP =
    Localhost
        <$> option
            auto
            ( short 'l'
                <> help "The local port to run on"
                <> showDefault
                <> metavar "INT"
            )

mkElmOpts :: ApiAuthority -> ElmOptions
mkElmOpts (Localhost p) =
    defElmOptions
        { urlPrefix = Static $ "http://localhost:" <> (T.show p) <> "/api"
        }
mkElmOpts (Host d) =
    defElmOptions
        { urlPrefix = Static $ "https://" <> d <> "/api"
        }
