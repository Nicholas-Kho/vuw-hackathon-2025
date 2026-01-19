module Domain.Server () where

import Api.Backend
import Servant
import TePapa.Convert

server :: Server BackendApi
server = serveStart :<|> serveExpand

serveExpand :: ExpandParams -> Handler [GraphAction]
serveExpand params = error "todo"

serveStart :: Handler InitialGameState
serveStart = error "todo"
