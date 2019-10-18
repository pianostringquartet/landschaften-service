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
  
  -- QueryEndpointConstraints is just String
  -- can you make more nested type already?
  -- 
  :<|> "query" :> ReqBody '[PlainText, JSON] QueryEndpointConstraints 
                :> Post '[ JSON] PaintingsResponse


-- check out auto derivation of Swagger docs via Servant;

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
        query_ conn "select distinct author, title, date, wga_jpg, type, school, timeframe, concepts from paintings" :: IO [PaintingRow]
    
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

    queryPaintings :: QueryEndpointConstraints -> Handler PaintingsResponse
    queryPaintings constraintsInfo =
      liftIO $
      fmap (PaintingsResponse . map paintingRowToPainting) $
      withResource conns $ \conn -> do
        _ <- print "queryPaintings called"
        _ <- print $ "plaintext arg given: " ++ constraintsInfo
        _ <- print $ "plainTextToConstraintsInfo result: " ++ (show (plainTextToConstraintsInfo constraintsInfo))
        uncurry (query conn) 
          (BQ.buildQuery $ 
            BQ.constraints (plainTextToConstraintsInfo constraintsInfo)) :: IO [PaintingRow]


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

