module TePapa.Env (
    getApiKey,
    getPort,
    getSeed,
    getSemaphore,
    getStaticPath,
    loadDotEnv,
) where

import qualified Configuration.Dotenv as Dotenv
import Control.Concurrent (QSem, newQSem)
import Data.Text
import System.Environment
import TePapa.Decode (ExternalId (..), MuseumResource (..), TePapaReference (..))
import Text.Read (readMaybe)

expectKey :: Maybe String -> String
expectKey (Just key) = key
expectKey Nothing =
    error
        "The environment variable 'API_KEY' doesn't exist. Make sure you have a .env file with this variable!"

getApiKey :: IO Text
getApiKey =
    pack . expectKey <$> lookupEnv "API_KEY"

loadDotEnv :: IO ()
loadDotEnv = Dotenv.loadFile Dotenv.defaultConfig

getSemaphore :: IO QSem
getSemaphore =
    lookupEnv "MAX_CONCURRENT_HTTP" >>= \case
        Nothing -> newQSem 8
        Just x ->
            case readMaybe @Int x of
                Nothing -> newQSem 8
                Just k -> newQSem k

getPort :: IO Int
getPort = do
    lookupEnv "PORT" >>= \case
        Nothing -> pure 8080
        Just rawPort ->
            case readMaybe @Int rawPort of
                Nothing -> pure 8080
                Just k ->
                    if k <= 2048
                        then error "Please pick a port number above 2048."
                        else pure k

defaultSeed :: TePapaReference
defaultSeed = TePapaReference{namespace = ObjectR, eid = ExternalId{unId = 1227923}}

getSeed :: IO TePapaReference
getSeed =
    lookupEnv "SEED" >>= \case
        Nothing -> pure defaultSeed
        Just s ->
            case parseTRef s of
                Nothing -> pure defaultSeed
                Just tref -> pure tref

parseTRef :: String -> Maybe TePapaReference
parseTRef s =
    case Prelude.words s of
        [nsRaw, idRaw] -> do
            ns <- parseNamespace nsRaw
            eid <- readMaybe @Int idRaw
            pure TePapaReference{namespace = ns, eid = ExternalId{unId = eid}}
        _not2words -> Nothing

parseNamespace :: String -> Maybe MuseumResource
parseNamespace s =
    case s of
        -- There's other variants, but they don't map to nodes
        "object" -> Just ObjectR
        "agent" -> Just AgentR
        "place" -> Just PlaceR
        _nomatch -> Nothing

getStaticPath :: IO FilePath
getStaticPath = do
    lookupEnv "STATIC_PATH" >>= \case
        Nothing -> pure "static/"
        Just p -> pure p
