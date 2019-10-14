{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Data.Aeson.Types                 (Parser (..), parse,
                                                   parseEither)
import           Data.Maybe                       (fromJust)
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import           Control.Exception                (bracket)
import           Control.Monad.IO.Class
import qualified Data.ByteString                  as BS
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
import           Network.HTTP.Client              (defaultManagerSettings,
                                                   newManager)
import           Servant.Client

import qualified BuildQuery                       as BQ
import           Domain

import           Data.ByteString.UTF8             (fromString)

import System.Environment
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import GHC.IO (IO)

type DBConnectionString = BS.ByteString

type QueryEndpointConstraints = String

type API
   = "artists" :> Get '[ JSON] ArtistsResponse
   :<|> "concepts" :> Get '[ JSON] ConceptNamesResponse
   :<|> "paintings" :> Get '[ JSON] PaintingsResponse
--   :<|> "query" :> ReqBody '[PlainText] QueryEndpointConstraints :> Post '[ JSON] PaintingsResponse
  :<|> "query" :> ReqBody '[PlainText, JSON] QueryEndpointConstraints :> Post '[ JSON] PaintingsResponse
--    :<|> "query" :> ReqBody '[PlainText] QueryEndpointConstraints :> Post '[ JSON] PaintingsResponse
--   :<|> "query" :> ReqBody '[ JSON] BQ.ConstraintsInfo :> Post '[ JSON] PaintingsResponse


api :: Proxy API
api = Proxy

-- move out the handlers to be REAL handlers,
-- that take REAL dependencies


-- stream-line the query calls etc.
server :: Pool Connection -> Server API
server conns = getArtists :<|> getConcepts :<|> getPaintings :<|> queryPaintings
  where
    getArtists :: Handler ArtistsResponse
    getArtists =
      liftIO $
      fmap (ArtistsResponse . map prAuthor) $
      withResource conns $ \conn ->
        query_ conn "select distinct author, title, date, wga_jpg, type, school, timeframe, concepts from paintings" :: IO [PaintingRow]
    -- this can just use the ConceptRow or some other abstraction
    getConcepts :: Handler ConceptNamesResponse
    getConcepts =
      liftIO $
      fmap (ConceptNamesResponse . map conceptName) $
      withResource conns $ \conn -> query_ conn "select distinct name from paintings_concepts" :: IO [ConceptNameRow]
    getPaintings :: Handler PaintingsResponse
    getPaintings =
      liftIO $
      fmap (PaintingsResponse . map paintingRowToPainting) $
      withResource conns $ \conn -> uncurry (query conn) BQ.sampleQuery :: IO [PaintingRow]

--    queryPaintings :: BQ.ConstraintsInfo -> Handler PaintingsResponse
    queryPaintings :: QueryEndpointConstraints -> Handler PaintingsResponse
    queryPaintings constraintsInfo =
      liftIO $
      fmap (PaintingsResponse . map paintingRowToPainting) $
      withResource conns $ \conn -> do
        _ <- print "queryPaintings called"
        _ <- print $ "plaintext arg given: " ++ constraintsInfo
        _ <- print $ "plainTextToConstraintsInfo result: " ++ (show (plainTextToConstraintsInfo constraintsInfo))
--        _ <- print $ "decoded: " ++ (fromJust $ (decodeStrict $ fromString constraintsInfo)) :: BQ.ConstraintsInfo
        uncurry (query conn) (BQ.buildQuery $ BQ.constraints (plainTextToConstraintsInfo constraintsInfo)) :: IO [PaintingRow]

plainTextToConstraintsInfo :: String -> BQ.ConstraintsInfo
plainTextToConstraintsInfo s = fromJust $ decodeStrict $ fromString s


runApp :: Int -> Pool Connection -> IO ()
runApp portNumber conns = do
  let port = portNumber
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on the following port " ++ show port)) $ defaultSettings
  runSettings settings =<< mkApp conns


-- | Allow Content-Type header with values other then allowed by simpleCors.
-- simpleCorsResourcePolicy is still constrained to "simple content types" with e.g. POST
-- so use a more advanced Cors policy?
corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
  where policy = simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}

mkApp :: Pool Connection -> IO Application
mkApp conns = return $
  corsWithContentType $
  provideOptions api $
  serve api $ server conns

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool
    (connectPostgreSQL connStr)
    close
    2 -- # stripes (?)
    60 -- unused connxns kept open for 60 seconds
    10 -- max conns per strip

--getArtists :: ClientM ArtistsResponse
--getConcepts :: ClientM ConceptNamesResponse
--getPaintings :: ClientM PaintingsResponse
----queryPaintings :: ConstraintsInfo -> ClientM PaintingsResponse
--queryPaintings :: QueryEndpointConstraints -> ClientM PaintingsResponse
--
--getArtists :<|> getConcepts :<|> getPaintings :<|> queryPaintings = client api


--d :: String
----d = "{\"constraints\": [{\"column\": \"name\", \"values\": [\"scarf\"]}]}"
----d = "{\"constraints\": [{\"column\": \"name\", \"values\": [\"scarf\", \"deer\"]}]}"
---- with multiple kinds of constraints:
--d = "{\"constraints\": [ {\"column\": \"timeframe\", \"values\": [\"1501-1550\"]}, {\"column\": \"school\", \"values\": [\"French\", \"Italian\", \"Spanish\", \"German\"]}, {\"column\": \"name\", \"values\": [\"scarf\", \"deer\"]}]}"
--myRun :: IO ()
--myRun = do
--  -- you could read this from some configuration file,
--  -- environment variable or somewhere else instead.
--  -- you will need to either change this connection string OR
--  -- set some environment variables (see
--  -- https://www.postgresql.org/docs/9.5/static/libpq-envars.html)
--  -- to point to a running PostgreSQL server for this example to work.
--  let connString = "postgresql://localhost:5432/bilder"
--  pool <- initConnectionPool connString
----  initDB connStr
--  mgr <- newManager defaultManagerSettings
--  bracket (forkIO $ runApp 8080 pool) killThread $ \_ -> do
--    ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
----      queryPaintings rtext
--        queryPaintings d
----      _ <- print qp
----      queryPaintings
----      PaintingsResponse []
--    print ms
--


--runMyQuery = do
--  dbUrl <- getEnv "DB_URL"
--  portNumber <- getEnv "PORT"
--  _ <- print $ "dburl is: " ++ dbUrl
--  _ <- print $ "portNumber is: " ++ portNumber
--  myPool <- initConnectionPool (fromString dbUrl)
--  res <-
--    withResource myPool $ \conn ->
--      query_ conn "select distinct author, title, wga_jpg, type, school, timeframe, concepts from paintings" :: IO [PaintingRow]
--  _ <- print (length res) -- all rows
--  _ <- print (head res) -- single row
--  _ <- print (prConcepts (head res)) -- just the raw json :)
--  _ <- print (parseEither parseConcepts (fromJust $ decodeStrict $ fromString (prConcepts (head res))))
--  putStrLn "Done."

--
--
--plainTextToConstraintsInfo :: String -> BQ.ConstraintsInfo
----plainTextToConstraintsInfo s = (BQ.ConstraintsInfo [BQ.Constraint "name" ["elderly"]])
--plainTextToConstraintsInfo s = fromJust $ decodeStrict $ fromString s
----(BQ.ConstraintsInfo [BQ.Constraint "name" ["elderly"]])