{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Handlers where

import           BuildQuery                         as BQ
import           Domain

import           Data.Aeson
import           Data.Aeson.Types                   (Parser (..), Value, parse,
                                                     parseEither)
import           Data.Maybe                         (fromJust)
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types

import           Type.Reflection                    (Typeable)

import           Control.Monad.IO.Class             (liftIO)
import           Data.ByteString.UTF8               (fromString)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import qualified Data.Text as Text
import Control.Monad (forM_, forM)

type QueryEndpointConstraints = String

-- if this decoding fails, then should return a 404
-- are there servant options to do this already?
plainTextToConstraintsInfo :: String -> BQ.ConstraintsInfo
plainTextToConstraintsInfo s = fromJust $ decodeStrict $ fromString s


parseConcepts :: Value -> Parser [Concept]
parseConcepts =
  withObject "concepts" $ \o -> do
    general <- o .: "general"
    general .: "concepts"

-- TODO: failing to decode database-stored json should return Error (ie 500),
-- not an empty list of concepts.
-- or: log the error, but don't crash the server?
optimisticDecodeConcepts :: String -> [Concept]
optimisticDecodeConcepts json =
  case parseEither parseConcepts $ fromJust $ decodeStrict $ fromString json of
    Left a  -> []
    Right b -> b

getArtists :: Pool Connection -> Handler ArtistsResponse
getArtists conns =
  liftIO $
  fmap (ArtistsResponse . map prAuthor) $ withResource conns $ \conn -> query_ conn BQ.namesQuery :: IO [PaintingRow]


-- can't use `:: IO [Concept]` because query string is just grabbing one field ("name")
--
getConcepts :: Pool Connection -> Handler ConceptNamesResponse
getConcepts conns =
  liftIO $
  fmap (ConceptNamesResponse . map conceptName) $
  withResource conns $ \conn -> query_ conn BQ.conceptsQuery :: IO [ConceptNameRow]

--getConcepts :: Pool Connection -> Handler ConceptNamesResponse
--getConcepts conns =
--  liftIO $
--  -- just return strings here; Concept is otherwise already something in your app
--  -- or, withdraw the rows as Concepts, then take the part you need
----return $ ConceptNamesResponse $
--  withResource conns $ \conn -> do
--    xs <- query_ conn BQ.conceptsQuery :: IO [String]
--    return $ ConceptNamesResponse $ forM xs $ \(conceptName) -> return (conceptName :: String)


queryPaintings :: Pool Connection -> QueryEndpointConstraints -> Handler PaintingsResponse
queryPaintings conns constraintsInfo =
  liftIO $
--  fmap (PaintingsResponse . map paintingRowToPainting) $
  fmap PaintingsResponse $
  withResource conns $ \conn -> do
    _ <- print "queryPaintings called"
    _ <- print $ "plaintext arg given: " ++ constraintsInfo
    _ <- print $ "plainTextToConstraintsInfo result: " ++ (show (plainTextToConstraintsInfo constraintsInfo))
    xs <-
        uncurry (query conn) (BQ.buildQuery $ BQ.constraints (plainTextToConstraintsInfo constraintsInfo))
    forM xs $ \(author, title, date, wga_jpg, genre, school, timeframe, concepts) ->
      return $ Painting author title date wga_jpg genre school timeframe (optimisticDecodeConcepts concepts)

-- fmap this over the xs?
--rowToPainting :: (String, String, String, String, Genre, School, String, String) -> Painting
--rowToPainting (author, title, date, wga_jpg, genre, school, timeframe, concepts) =
--  Painting author title date wga_jpg genre school timeframe (optimisticDecodeConcepts concepts)


newtype ConceptNamesResponse =
  ConceptNamesResponse
    { conceptNames :: [String]
    }
  deriving (Eq, Show, Read, Generic)

instance ToJSON ConceptNamesResponse

instance FromJSON ConceptNamesResponse

newtype ConceptNameRow =
  ConceptNameRow
    { conceptName :: String
    }
  deriving (Eq, Show, Read, Generic)

instance ToJSON ConceptNameRow

instance FromJSON ConceptNameRow

instance FromRow ConceptNameRow where
  fromRow = ConceptNameRow <$> field

newtype ArtistsResponse =
  ArtistsResponse
    { artists :: [String]
    }
  deriving (Eq, Show, Read, Generic)

instance ToJSON ArtistsResponse

instance FromJSON ArtistsResponse

newtype PaintingsResponse =
  PaintingsResponse
    { paintings :: [Painting]
    }
  deriving (Eq, Show, Read, Generic, Typeable)

instance ToJSON PaintingsResponse

instance FromJSON PaintingsResponse

newtype ConceptRow =
  ConceptRow
    { conceptColumn :: String
    }
  deriving (Eq, Show, Read, Generic, Typeable)

--instance ToJSON ConceptRow
--instance FromJSON ConceptRow
instance FromRow ConceptRow where
  fromRow = ConceptRow <$> field

--instance FromField ConceptRow where
--  fromField f mdata = fromJSONField
data PaintingRow =
  PaintingRow
    { prAuthor    :: String
    , prTitle     :: String
    , prDate      :: String
    , prJpg       :: String
    , prGenre     :: Genre
    , prSchool    :: School
    , prTimeframe :: String
    , prConcepts  :: String -- a json-formatted string, as in ConceptRow
    }
  deriving (Eq, Show, Read, Generic, Typeable)

instance FromRow PaintingRow where
  fromRow = PaintingRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
