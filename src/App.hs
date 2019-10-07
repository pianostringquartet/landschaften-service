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

--http://hackage.haskell.org/package/warp-3.3.2/docs/Network-Wai-Handler-Warp.html
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

type DBConnectionString = BS.ByteString

type API
   = "artists" :> Get '[ JSON] ArtistsResponse :<|> "concepts" :> Get '[ JSON] ConceptNamesResponse :<|> "paintings" :> Get '[ JSON] PaintingsResponse :<|> "query" :> ReqBody '[ JSON] BQ.ConstraintsInfo :> Post '[ JSON] PaintingsResponse

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server conns = getArtists :<|> getConcepts :<|> getPaintings :<|> queryPaintings
  where
    getArtists :: Handler ArtistsResponse
    getArtists =
      liftIO $
      fmap (ArtistsResponse . map prAuthor) $
      withResource conns $ \conn ->
        query_ conn "select distinct author, title, wga_jpg, type, school, timeframe, concepts from paintings" :: IO [PaintingRow]
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
    queryPaintings :: BQ.ConstraintsInfo -> Handler PaintingsResponse
    queryPaintings constraintsInfo =
      liftIO $
      fmap (PaintingsResponse . map paintingRowToPainting) $
      withResource conns $ \conn ->
        uncurry (query conn) (BQ.buildQuery $ BQ.constraints constraintsInfo) :: IO [PaintingRow]

--        query_ conn "select distinct author, title, wga_jpg, type, school, timeframe, concepts from paintings" :: IO [PaintingRow]
runApp :: Pool Connection -> IO ()
runApp conns = do
  let port = 8080
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on the following port " ++ show port)) $ defaultSettings
  runSettings settings =<< mkApp conns

mkApp :: Pool Connection -> IO Application
mkApp conns = return $ serve api $ server conns

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool
    (connectPostgreSQL connStr)
    close
    2 -- # stripes (?)
    60 -- unused connxns kept open for 60 seconds
    10 -- max conns per strip


runMyQuery = do
  dbUrl <- getEnv "DB_URL"
  _ <- print $ "dburl is: " ++ dbUrl

  myPool <- initConnectionPool (fromString dbUrl)
  res <-
    withResource myPool $ \conn ->
      query_ conn "select distinct author, title, wga_jpg, type, school, timeframe, concepts from paintings" :: IO [PaintingRow]
  _ <- print (length res) -- all rows
  _ <- print (head res) -- single row
  _ <- print (prConcepts (head res)) -- just the raw json :)
  _ <- print (parseEither parseConcepts (fromJust $ decodeStrict $ fromString (prConcepts (head res))))
  putStrLn "Done."

