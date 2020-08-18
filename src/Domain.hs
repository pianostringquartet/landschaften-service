{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Domain where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Char8                (ByteString)
import           Data.Maybe                           (fromJust)
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       fromJSONField)
import           Database.PostgreSQL.Simple.Types
import           GHC.Generics
import           Type.Reflection                      (Typeable)

data Painting =
  Painting
    { paintingId :: Int
    , author     :: String -- better?: make a custom type Author
    , title      :: String
    , date       :: String
    , jpg        :: String -- WGA_JPEG, not the Cloudinary jpeg
    , genre      :: Genre -- aka "type" in the db tables
    , school     :: School
    , timeframe  :: String
    , concepts   :: [Concept]
    }
  deriving (Eq, Show, Read, Generic, Typeable)

instance ToJSON Painting

instance FromJSON Painting

data Concept = Concept { name  :: String , value :: Float }
  deriving (Eq, Show, Read, Ord, Generic, Typeable)

instance ToJSON Concept

instance FromJSON Concept

-- A concept and how often it appears in paintings set
type ConceptFrequency = (String, Double)

newtype GenreWrapper =
  GenreWrapper String

data Genre
  = Mythological
  | Interior
  | Landscape
  | Study
  | GenreGenre
  | Religious
  | OtherGenre
  | StillLife
  | Historical
  | Portrait
  deriving (Eq, Show, Read, Generic)

-- Aeson provides a deriving-helper function
-- https://artyom.me/aeson#records-and-json
-- Generics: customising field names
-- e.g.
--instance ToJSON Person where
--  toJSON = genericToJSON defaultOptions {
--             fieldLabelModifier = <your modifier fn here> }
-- UPDATE: difficult to use this because of mappings like "GenreGenre -> genre" and "StillLife -> still-life"
instance ToJSON Genre where
  toJSON a =
    case a of
      Mythological -> "mythological"
      Interior     -> "interior"
      Landscape    -> "landscape"
      Study        -> "study"
      GenreGenre   -> "genre"
      Religious    -> "religious"
      OtherGenre   -> "other"
      StillLife    -> "still-life"
      Historical   -> "historical"
      Portrait     -> "portrait"

instance FromJSON Genre where
  parseJSON (String "mythological") = return Mythological
  parseJSON (String "interior")     = return Interior
  parseJSON (String "landscape")    = return Landscape
  parseJSON (String "study")        = return Study
  parseJSON (String "genre")        = return GenreGenre
  parseJSON (String "religious")    = return Religious
  parseJSON (String "other")        = return OtherGenre
  parseJSON (String "still-life")   = return StillLife
  parseJSON (String "historical")   = return Historical
  parseJSON (String "portrait")     = return Portrait
  parseJSON _                       = error "Did not recognize the Genre"

instance FromField Genre where
  fromField f mdata = return genre
    where
      genre =
        case mdata of
          Just "mythological" -> Mythological
          Just "interior"     -> Interior
          Just "landscape"    -> Landscape
          Just "study"        -> Study
          Just "genre"        -> GenreGenre
          Just "religious"    -> Religious
          Just "other"        -> OtherGenre
          Just "still-life"   -> StillLife
          Just "historical"   -> Historical
          Just "portrait"     -> Portrait
          _                   -> OtherGenre

data School
  = German
  | Italian
  | Danish
  | Flemish
  | Dutch
  | Netherlandish
  | Swiss
  | Other
  | Russian
  | English
  | Austrian
  | Scottish
  | Bohemian
  | French
  | Spanish
  | Belgian
  | Hungarian
  | American
  | Polish
  | Norwegian
  | Swedish
  | Irish
  | Finnish
  | Portuguese
  | Greek
  | Catalan
  deriving (Eq, Show, Read, Generic)

instance ToJSON School

instance FromJSON School

-- Can this also be made smaller or simpler?
-- e.g. just handle the _ case?
instance FromField School where
  fromField f mdata = return school
    where
      school =
        case mdata of
          Just "German"        -> German
          Just "Italian"       -> Italian
          Just "Danish"        -> Danish
          Just "Flemish"       -> Flemish
          Just "Dutch"         -> Dutch
          Just "Netherlandish" -> Netherlandish
          Just "Swiss"         -> Swiss
          Just "Other"         -> Other
          Just "Russian"       -> Russian
          Just "English"       -> English
          Just "Austrian"      -> Austrian
          Just "Scottish"      -> Scottish
          Just "Bohemian"      -> Bohemian
          Just "French"        -> French
          Just "Spanish"       -> Spanish
          Just "Belgian"       -> Belgian
          Just "Hungarian"     -> Hungarian
          Just "American"      -> American
          Just "Polish"        -> Polish
          Just "Norwegian"     -> Norwegian
          Just "Swedish"       -> Swedish
          Just "Irish"         -> Irish
          Just "Finnish"       -> Finnish
          Just "Portuguese"    -> Portuguese
          Just "Greek"         -> Greek
          Just "Catalan"       -> Catalan
          _                    -> Other
