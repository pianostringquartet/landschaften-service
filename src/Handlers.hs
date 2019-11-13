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

--import           Data.Set                           hiding (filter, map, take)
import qualified Data.Set                           as D
import           Data.Text.Encoding                 (encodeUtf8)
import           Data.Text.Internal                 (Text)

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

paintingsLimit :: Int
--paintingsLimit = 200
paintingsLimit = 51 -- should be determined by frontend-sent param...

minimumConceptCertainty :: Float
minimumConceptCertainty = 0.85

-- We're only interested in the n-many most frequent concepts
nMostFrequentConcepts :: Int
nMostFrequentConcepts = 50

type PaintingRow = (Int, String, String, String, String, Genre, School, String, String)

rowToPainting :: PaintingRow -> Painting
rowToPainting (id, author, title, date, wga_jpg, genre, school, timeframe, concepts) =
  Painting id author title date wga_jpg genre school timeframe (optimisticDecodeConcepts concepts)

rowsToPaintingsResponse :: [PaintingRow] -> PaintingsResponse
rowsToPaintingsResponse rows =
  PaintingsResponse
    (take paintingsLimit paintings)
    (map paintingId paintings)
    (takeNMostFrequentConcepts2
       nMostFrequentConcepts
       (getConceptFrequencies
            (concatMap concepts paintingsSampleSet)
            (length paintingsSampleSet))
      )
    (length paintings)
  where
    paintings = map rowToPainting rows
    sampleSize = getSampleSize (length paintings)
    paintingsSampleSet = take (getSampleSize (length paintings)) paintings

cs :: [Concept]
cs = [Concept "love" 0.80, Concept "joy" 0.85, Concept "pain" 0.89, Concept "affectless" 0.95]

-- test: getSampleSize 30000 -> 379
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


frequency :: Concept -> [Concept] -> Int -> Double
frequency c cs totalPaintings = fromIntegral appearances / fromIntegral totalPaintings
  where
    conceptName = name c
    conceptNames = map name cs
    appearances = length (filter (== conceptName) conceptNames)

frequency2 :: String -> [String] -> Int -> Double
frequency2 conceptName conceptNames totalPaintings = fromIntegral appearances / fromIntegral totalPaintings
  where
    appearances = length (filter (== conceptName) conceptNames)

-- sort the CFs from highest->lowest frequency,
-- then take int-many
-- this logic is correct... but the earlier logic seems wrong.
takeNMostFrequentConcepts :: Int -> [ConceptFrequency] -> [ConceptFrequency]
--takeNMostFrequentConcepts n cfs = take n $ reverse $ sortBy (compare `on` snd) cfs
takeNMostFrequentConcepts n cfs = take n $ sortBy (flip compare `on` snd) cfs


-- can you combine these operations, e.g. sort while turning set into a list?
-- is haskell doing that under the hood?
takeNMostFrequentConcepts2 :: Int -> D.Set ConceptFrequency -> [ConceptFrequency]
takeNMostFrequentConcepts2 n cfs = take n $ sortBy (flip compare `on` snd) (D.toList cfs)

-- want to turn these ConceptFrequencies into a set, or otherwise remove duplicates;
-- i.e. calculate the frequency of every concept FIRST, then remove any duplicates results


-- possibly want D.Sequence type here, for finite lists?
-- does this set actually help?
--getConceptFrequencies :: [Concept] -> Int -> D.Set ConceptFrequency -- D.Set Concept --
getConceptFrequencies :: [Concept] -> Int -> D.Set ConceptFrequency -- D.Set Concept --
getConceptFrequencies concepts totalPaintings =
   D.map (\conceptName -> (conceptName, frequency2 conceptName conceptNames totalPaintings)) uniqueConceptNames
  where
    conceptNames = map name concepts
    uniqueConceptNames = D.fromList conceptNames

--getConceptFrequencies cs totalPaintings = toList . fromList $ map (\c -> (name c, frequency c cs totalPaintings)) cs
--- ^^^^ when no `toList . fromList`, then
-- better implementation: if concept-already-seen then don't calculate frequency
-- i.e. so can't do a map, because not every member of the list will be used
-- instead of iterating through all members, just iterate through unique members
--highCertaintyConcepts :: [Concept] -> [Concept]
--highCertaintyConcepts = filter (\concept -> value concept >= minimumConceptCertainty)
-- this is true, but if two different paintings each have concept C
-- and in each case have C rated above the threshold,
-- we'll end up with multiple versions of that same concept
conceptsWithCertaintyGTE :: Float -> [Painting] -> [Concept]
conceptsWithCertaintyGTE certaintyGTE ps = filter (\concept -> value concept >= certaintyGTE) $ concatMap concepts ps

getArtists :: Pool Connection -> Handler ArtistsResponse
getArtists conns =
  liftIO $
  fmap (ArtistsResponse . map artistName) $
  withResource conns $ \conn -> query_ conn BQ.namesQuery :: IO [ArtistNameRow]

-- can't use `:: IO [Concept]` because query string is just grabbing one field ("name");
-- don't want to attach a temporary name to the field (as ConceptNameRow does);
-- withdrawing and casting via `:: String` throws PostgreSQL error
getConcepts :: Pool Connection -> Handler ConceptNamesResponse
getConcepts conns =
  liftIO $
  fmap (ConceptNamesResponse . map conceptName) $
  withResource conns $ \conn -> query_ conn BQ.conceptsQuery :: IO [ConceptNameRow]

queryPaintings :: Pool Connection -> Text -> Handler PaintingsResponse
queryPaintings conns constraintsInfo =
  case eitherDecodeStrict $ encodeUtf8 constraintsInfo of
    Left _ -> throwError err422
    Right decodedConstraints ->
      liftIO $
      fmap rowsToPaintingsResponse $
      withResource conns $ \conn -> uncurry (query conn) (BQ.buildQuery $ BQ.constraints decodedConstraints)

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

-- for now, bundle all this together into a single request
-- needs to now be :paintings, :painting ids
data PaintingsResponse =
  PaintingsResponse
    { paintings          :: [Painting]
    , paintingIds        :: [Int]
    , conceptFrequencies :: [ConceptFrequency] -- D.Set ConceptFrequency
    , total :: Int
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
