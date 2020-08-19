{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import qualified Data.ByteString                        as BS
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Servant.Options

import           GHC.IO                                 (IO)

import           Data.Text.Internal                     (Text)
import           Handlers

type DBConnectionString = BS.ByteString

type API
   = "artists" :> Get '[ JSON] ArtistsResponse :<|> "concepts" :> Get '[ JSON] ConceptNamesResponse :<|> "query" :> ReqBody '[ PlainText, JSON] Text :> Post '[ JSON] PaintingsResponse

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server conns = getArtists conns :<|> getConcepts conns :<|> queryPaintings conns

runApp :: Int -> Pool Connection -> IO ()
runApp portNumber conns = do
  let port = portNumber
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on the following port " ++ show port)) $ defaultSettings
  runSettings settings =<< mkApp conns

corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}

mkApp :: Pool Connection -> IO Application
mkApp conns = return $ corsWithContentType $ provideOptions api $ serve api $ server conns

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool
    (connectPostgreSQL connStr)
    close
    2 -- # stripes (?)
    60 -- unused connections kept open for 60 seconds
    10 -- max conns per strip
