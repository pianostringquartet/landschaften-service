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

import Control.Exception (bracket)
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

import Handlers

type DBConnectionString = BS.ByteString



type API
   = "artists" :> Get '[ JSON] ArtistsResponse
   :<|> "concepts" :> Get '[ JSON] ConceptNamesResponse

  -- QueryEndpointConstraints is just String
  -- can you make more nested type already?
  -- maybe can't, because endpoint is receiving 'content-type: plaintext' rather than json?
  :<|> "query" :> ReqBody '[PlainText, JSON] QueryEndpointConstraints
                :> Post '[ JSON] PaintingsResponse

-- check out auto derivation of Swagger docs via Servant;
api :: Proxy API
api = Proxy

--getArtists :: Pool Connection -> Handler ArtistsResponse
--getArtists conns = liftIO $
--      fmap (ArtistsResponse . map prAuthor) $
--      withResource conns $ \conn ->
--        query_ conn BQ.namesQuery  :: IO [PaintingRow]
--
--getConcepts :: Pool Connection -> Handler ConceptNamesResponse
--getConcepts conns = liftIO $
--      fmap (ConceptNamesResponse . map conceptName) $
--      withResource conns $ \conn -> query_ conn BQ.conceptsQuery :: IO [ConceptNameRow]
--
--queryPaintings :: Pool Connection -> QueryEndpointConstraints -> Handler PaintingsResponse
--queryPaintings conns constraintsInfo = liftIO $
--      fmap (PaintingsResponse . map paintingRowToPainting) $
--      withResource conns $ \conn -> do
--        _ <- print "queryPaintings called"
--        _ <- print $ "plaintext arg given: " ++ constraintsInfo
--        _ <- print $ "plainTextToConstraintsInfo result: " ++ (show (plainTextToConstraintsInfo constraintsInfo))
--        uncurry (query conn)
--          (BQ.buildQuery $
--            BQ.constraints (plainTextToConstraintsInfo constraintsInfo)) :: IO [PaintingRow]

server :: Pool Connection -> Server API
server conns = getArtists conns
               :<|> getConcepts conns
               :<|> queryPaintings conns
--
--plainTextToConstraintsInfo :: String -> BQ.ConstraintsInfo
--plainTextToConstraintsInfo s = fromJust $ decodeStrict $ fromString s
--
--paintingRowToPainting :: PaintingRow -> Painting
--paintingRowToPainting pr =
--  Painting
--    (prAuthor pr)
--    (prTitle pr)
--    (prDate pr)
--    (prJpg pr)
--    (prGenre pr)
--    (prSchool pr)
--    (prTimeframe pr)
--    (optimisticDecodeConcepts (prConcepts pr))
--
--parseConcepts :: Value -> Parser [Concept]
--parseConcepts =
--  withObject "concepts" $ \o -> do
--    general <- o .: "general"
--    general .: "concepts"
--
--optimisticDecodeConcepts :: String -> [Concept]
--optimisticDecodeConcepts j =
--  case parseEither parseConcepts $ fromJust $ decodeStrict $ fromString j of
--    Left a  -> []
--    Right b -> b



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

