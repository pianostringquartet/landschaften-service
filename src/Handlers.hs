{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Handlers where

import           BuildQuery                         as BQ
import           Domain

import           Data.Aeson
import           Data.Aeson.Types                   (Parser (..), Value,
                                                     parseEither)
import           Data.Pool
import           Database.PostgreSQL.Simple

import           Type.Reflection                    (Typeable)

import           Control.Monad.IO.Class             (liftIO)
import           Data.ByteString.UTF8               (fromString)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           GHC.Generics
import           Servant

import           System.IO

import           Data.Function                      (on)
import           Data.List                          (sortBy, nub)

import qualified Data.Set                           as D
import           Data.Text.Encoding                 (encodeUtf8)
import           Data.Text.Internal                 (Text)


-- ****************
-- HELPERS
-- ****************

parseConcepts :: Value -> Parser [Concept]
parseConcepts =
  withObject "concepts" $ \o -> do
    general <- o .: "general"
    general .: "concepts"

-- TODO: Log specific painting(s) whose concepts were not decodable
optimisticDecodeConcepts :: String -> [Concept]
optimisticDecodeConcepts json =
  case eitherDecodeStrict (fromString json) >>= parseEither parseConcepts of
    Left _  -> []
    Right b -> b

-- | Turn paintings into a response for frontend.   
paintingsToPaintingsResponse :: Int -> Int -> [Painting] -> PaintingsResponse
paintingsToPaintingsResponse limit nMostFrequent paintings =
  PaintingsResponse
      (take limit paintings) -- send a limited number of full-data paintings
      (map paintingId paintings) -- send all paintings' ids
      nMostFrequentConcepts -- send only n-many 'most frequent' concepts
    where
      sampleSize = getSampleSize (length paintings)
      paintingsSampleSet = take sampleSize paintings
      conceptFrequencies = getConceptFrequencies
                               (concatMap concepts paintingsSampleSet)
                               (length paintingsSampleSet)
      nMostFrequentConcepts = take nMostFrequent
                                   (sortBy (flip compare `on` snd) (D.toList conceptFrequencies))

-- 'How frequently, as a percentage, does X appears in the set?'
frequency :: String -> [String] -> Int -> Double
frequency conceptName conceptNames totalPaintings =
  fromIntegral appearances / fromIntegral totalPaintings
  where
    appearances = length (filter (== conceptName) conceptNames)

-- A concept's frequency is how frequently the concept appears in a set of paintings;
-- e.g. if 10 paintings total, and 2 paintings have concept C, then concept C's frequency is 20%
getConceptFrequencies :: [Concept] -> Int -> D.Set ConceptFrequency
getConceptFrequencies concepts totalPaintings =
   D.map
    (\conceptName -> (conceptName, frequency conceptName conceptNames totalPaintings))
    uniqueConceptNames
  where
    conceptNames = map name concepts
    uniqueConceptNames = D.fromList conceptNames

-- We generally work with sample sizes,
-- which are easier to
getSampleSize :: Int -> Int
getSampleSize population =
  round $ numerator / denominator
  where n = fromIntegral population
        errorMargin = 0.05 -- 5% margin of error
        z = 1.96 -- for a 95% confidence level
        sigma = 0.5 -- standard deviation
        zS = (z^2) * (sigma^2)
        numerator = zS * (n / (n - 1))
        denominator = errorMargin^2 + (zS / (n - 1))


-- ****************
-- HANDLERS
-- ****************

getArtists :: Pool Connection -> Handler ArtistsResponse
getArtists conns =
  liftIO $
  fmap (ArtistsResponse . map artistName) $
  withResource conns $ \conn -> query_ conn BQ.namesQuery :: IO [ArtistNameRow]

-- TODO: eliminate the ConceptNameRow middleware type;
-- can't use `:: IO [Concept]` because query string is just grabbing one field ("name");
-- don't want to attach a temporary name to the field (as ConceptNameRow does);
-- withdrawing and casting via `:: String` throws PostgreSQL error
getConcepts :: Pool Connection -> Handler ConceptNamesResponse
getConcepts conns =
  liftIO $
  fmap (ConceptNamesResponse . map conceptName) $
  withResource conns $ \conn -> query_ conn BQ.conceptsQuery :: IO [ConceptNameRow]


-- TODO: receive from frontend OR make function of sample size
-- More than 200 paintings is too much to visually inspect
paintingsLimit :: Int
paintingsLimit = 200

nManyConcepts :: Int
nManyConcepts = 50

queryPaintings :: Pool Connection -> Text -> Handler PaintingsResponse
queryPaintings conns constraintsInfo =
  case eitherDecodeStrict $ encodeUtf8 constraintsInfo of
    Left _ -> throwError err422
    Right decodedConstraints ->
      liftIO $
      fmap (paintingsToPaintingsResponse paintingsLimit nManyConcepts . map rowToPainting) $
      withResource conns $ \conn -> uncurry (query conn) (BQ.buildQuery $ BQ.constraints decodedConstraints)
  where
    rowToPainting (id, author, title, date, wga_jpg, genre, school, timeframe, concepts) =
      Painting id author title date wga_jpg genre school timeframe (optimisticDecodeConcepts concepts)


-- ****************
-- DATA TYPES
-- ****************

type PaintingRow = (Int, String, String, String, String, Genre, School, String, String)

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

newtype ArtistNameRow =
  ArtistNameRow
    { artistName :: String
    }
  deriving (Eq, Show, Read, Generic)

instance FromRow ArtistNameRow where
  fromRow = ArtistNameRow <$> field

data PaintingsResponse =
  PaintingsResponse
    { paintings          :: [Painting]
    , paintingIds        :: [Int]
    , conceptFrequencies :: [ConceptFrequency]
    }
  deriving (Eq, Show, Read, Generic, Typeable)

instance ToJSON PaintingsResponse

instance FromJSON PaintingsResponse

newtype ConceptRow =
  ConceptRow
    { conceptColumn :: String
    }
  deriving (Eq, Show, Read, Generic, Typeable)

instance FromRow ConceptRow where
  fromRow = ConceptRow <$> field
