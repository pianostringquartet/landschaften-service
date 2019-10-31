{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           App
import           Data.ByteString.UTF8 (fromString)
import           System.Environment

main :: IO ()
main
--  let connStr = "postgresql://localhost:5432/bilder"
 = do
  portNumber <- getEnv "PORT"
  connStr <- getEnv "DB_URL"
  pool <- initConnectionPool (fromString connStr)
  runApp (read portNumber) pool
