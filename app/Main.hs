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
--type DBConnectionString = BS.ByteString

getArtists :: ClientM ArtistsResponse
getConcepts :: ClientM ConceptNamesResponse
getPaintings :: ClientM PaintingsResponse
queryPaintings :: ConstraintsInfo -> ClientM PaintingsResponse

getArtists :<|> getConcepts :<|> getPaintings :<|> queryPaintings = client api

main :: IO ()
main = do
--  let connStr = "postgresql://localhost:5432/bilder"
  portNumber <- getEnv "PORT"
  connStr <- getEnv "DB_URL"
  pool <- initConnectionPool (fromString connStr)
  runApp (read portNumber) pool
