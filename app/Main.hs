{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           App
import Domain
import Servant.Client
import Servant
import BuildQuery

import qualified Data.ByteString                  as BS
import System.Environment

import           Data.ByteString.UTF8             (fromString)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
--type DBConnectionString = BS.ByteString

getArtists :: ClientM ArtistsResponse
getConcepts :: ClientM ConceptNamesResponse
getPaintings :: ClientM PaintingsResponse
--queryPaintings :: ConstraintsInfo -> ClientM PaintingsResponse
queryPaintings :: QueryEndpointConstraints -> ClientM PaintingsResponse

getArtists :<|> getConcepts :<|> getPaintings :<|> queryPaintings = client api

main :: IO ()
main = do
--  let connStr = "postgresql://localhost:5432/bilder"
  portNumber <- getEnv "PORT"
  connStr <- getEnv "DB_URL"
  pool <- initConnectionPool (fromString connStr)
  runApp (read portNumber) pool

--myRun :: String -> IO ()
--myRun rtext = do
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
--      queryPaintings rtext
----      _ <- print qp
----      queryPaintings
----      PaintingsResponse []
--      
--      
--    print ms