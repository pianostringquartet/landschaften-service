{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           App
import Domain
import Servant.Client
import Servant
import BuildQuery

getArtists :: ClientM ArtistsResponse
getConcepts :: ClientM ConceptNamesResponse
getPaintings :: ClientM PaintingsResponse
queryPaintings :: ConstraintsInfo -> ClientM PaintingsResponse

getArtists :<|> getConcepts :<|> getPaintings :<|> queryPaintings = client api

main :: IO ()
main = do
  let connStr = "postgresql://localhost:5432/bilder"
  pool <- initConnectionPool connStr
  runApp pool
