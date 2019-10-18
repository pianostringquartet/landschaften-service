{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Domain where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Char8                (ByteString)
import           Data.Maybe                           (fromJust)
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       fromJSONField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField   (ToField (..))
import           Database.PostgreSQL.Simple.ToRow     (ToRow (..))
import           Database.PostgreSQL.Simple.Types
import           GHC.Generics
import           Type.Reflection                      (Typeable)

import           Data.Aeson.Types                     (Parser (..), parse,
                                                       parseEither)
import           Data.ByteString.UTF8                 (fromString)


-- separate endpoint-return concepts from core business domain concepts

-- for the /concepts endpoint, to list all concepts 
newtype ConceptNamesResponse = ConceptNamesResponse {
  conceptNames :: [String]
} deriving (Eq, Show, Read, Generic)

instance ToJSON ConceptNamesResponse

instance FromJSON ConceptNamesResponse

newtype ConceptNameRow = ConceptNameRow {
  conceptName :: String
} deriving (Eq, Show, Read, Generic)
  
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

data Painting =
  Painting
    { author    :: String -- make a custom type?
    , title     :: String
    , date :: String
    , jpg       :: String -- WGA_JPEG, not the Cloudinary jpeg
    , genre     :: Genre -- aka "type" in the db tables
    , school    :: School
    , timeframe :: String
    , concepts  :: [Concept]
    }
  deriving (Eq, Show, Read, Generic, Typeable)

instance ToJSON Painting

instance FromJSON Painting

data ConceptRow =
  ConceptRow
    { conceptColumn :: String -- ie the json
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
    , prDate :: String
    , prJpg       :: String
    , prGenre     :: Genre
    , prSchool    :: School
    , prTimeframe :: String
    , prConcepts  :: String -- a json-formatted string, as in ConceptRow
    }
  deriving (Eq, Show, Read, Generic, Typeable)

instance FromRow PaintingRow where
  fromRow = PaintingRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data Concept =
  Concept
    { name  :: String
    , value :: Float
    }
  deriving (Eq, Show, Read, Generic, Typeable)

instance ToJSON Concept

instance FromJSON Concept

parseConcepts :: Value -> Parser [Concept]
parseConcepts =
  withObject "concepts" $ \o -> do
    general <- o .: "general"
    contents <- general .: "concepts"
    return contents

optimisticDecodeConcepts :: String -> [Concept]
optimisticDecodeConcepts j =
  case parseEither parseConcepts $ fromJust $ decodeStrict $ fromString j of
    Left a  -> []
    Right b -> b

paintingRowToPainting :: PaintingRow -> Painting
paintingRowToPainting pr =
  Painting
    (prAuthor pr)
    (prTitle pr)
    (prDate pr)
    (prJpg pr)
    (prGenre pr)
    (prSchool pr)
    (prTimeframe pr)
    (optimisticDecodeConcepts (prConcepts pr))


-- alternatively: use newtype, if don't need type safety
-- would be especially good for Author and Concept
newtype GenreWrapper = GenreWrapper String 

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

instance ToField Genre where
  toField mdata =
    case mdata of
      Mythological -> toField ("mythological" :: String)
      Interior     -> toField ("interior" :: String)
      Landscape    -> toField ("landscape" :: String)
      Study        -> toField ("study" :: String)
      GenreGenre   -> toField ("genre" :: String)
      Religious    -> toField ("religious" :: String)
      OtherGenre   -> toField ("other" :: String)
      StillLife    -> toField ("still-life" :: String)
      Historical   -> toField ("historical" :: String)
      Portrait     -> toField ("portrait" :: String)

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

--instance ToJSON School where
--  toJSON a =
--    case a of
--      German        -> "German"
--      Italian       -> "Italian"
--      Danish        -> "Danish"
--      Flemish       -> "Flemish"
--      Dutch         -> "Dutch"
--      Netherlandish -> "Netherlandish"
--      Swiss         -> "Swiss"
--      Other         -> "Other"
--      Russian       -> "Russian"
--      English       -> "English"
--      Austrian      -> "Austrian"
--      Scottish      -> "Scottish"
--      Bohemian      -> "Bohemian"
--      French        -> "French"
--      Spanish       -> "Spanish"
--      Belgian       -> "Belgian"
--      Hungarian     -> "Hungarian"
--      American      -> "American"
--      Polish        -> "Polish"
--      Norwegian     -> "Norwegian"
--      Swedish       -> "Swedish"
--      Irish         -> "Irish"
--      Finnish       -> "Finnish"
--      Portuguese    -> "Portuguese"
--      Greek         -> "Greek"
--      Catalan       -> "Catalan"

--instance FromJSON School where
--  parseJSON (String "German")        = return German
--  parseJSON (String "Italian")       = return Italian
--  parseJSON (String "Danish")        = return Danish
--  parseJSON (String "Flemish")       = return Flemish
--  parseJSON (String "Dutch")         = return Dutch
--  parseJSON (String "Netherlandish") = return Netherlandish
--  parseJSON (String "Swiss")         = return Swiss
--  parseJSON (String "Other")         = return Other
--  parseJSON (String "Russian")       = return Russian
--  parseJSON (String "English")       = return English
--  parseJSON (String "Austrian")      = return Austrian
--  parseJSON (String "Scottish")      = return Scottish
--  parseJSON (String "Bohemian")      = return Bohemian
--  parseJSON (String "French")        = return French
--  parseJSON (String "Spanish")       = return Spanish
--  parseJSON (String "Belgian")       = return Belgian
--  parseJSON (String "Hungarian")     = return Hungarian
--  parseJSON (String "American")      = return American
--  parseJSON (String "Polish")        = return Polish
--  parseJSON (String "Norwegian")     = return Norwegian
--  parseJSON (String "Swedish")       = return Swedish
--  parseJSON (String "Irish")         = return Irish
--  parseJSON (String "Finnish")       = return Finnish
--  parseJSON (String "Portuguese")    = return Portuguese
--  parseJSON (String "Greek")         = return Greek
--  parseJSON (String "Catalan")       = return Catalan
--  parseJSON _                        = error "Did not recognize the School"

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


instance ToField School where
  toField mdata =
    case mdata of
      German        -> toField ("German" :: String)
      Italian       -> toField ("Italian" :: String)
      Danish        -> toField ("Danish" :: String)
      Flemish       -> toField ("Flemish" :: String)
      Dutch         -> toField ("Dutch" :: String)
      Netherlandish -> toField ("Netherlandish" :: String)
      Swiss         -> toField ("Swiss" :: String)
      Other         -> toField ("Other" :: String)
      Russian       -> toField ("Russian" :: String)
      English       -> toField ("English" :: String)
      Austrian      -> toField ("Austrian" :: String)
      Scottish      -> toField ("Scottish" :: String)
      Bohemian      -> toField ("Bohemian" :: String)
      French        -> toField ("French" :: String)
      Spanish       -> toField ("Spanish" :: String)
      Belgian       -> toField ("Belgian" :: String)
      Hungarian     -> toField ("Hungarian" :: String)
      American      -> toField ("American" :: String)
      Polish        -> toField ("Polish" :: String)
      Norwegian     -> toField ("Norwegian" :: String)
      Swedish       -> toField ("Swedish" :: String)
      Irish         -> toField ("Irish" :: String)
      Finnish       -> toField ("Finnish" :: String)
      Portuguese    -> toField ("Portuguese" :: String)
      Greek         -> toField ("Greek" :: String)
      Catalan       -> toField ("Catalan" :: String)
      _             -> toField ("Other" :: String)
