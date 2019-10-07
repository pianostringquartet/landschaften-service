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
    , prJpg       :: String
    , prGenre     :: Genre
    , prSchool    :: School
    , prTimeframe :: String
    , prConcepts  :: String -- a json-formatted string, as in ConceptRow
    }
  deriving (Eq, Show, Read, Generic, Typeable)

-- the ConceptRow field itself is not complicated
-- and FromRow for ConceptRow "just worked"
instance FromRow PaintingRow where
  fromRow = PaintingRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field

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
    (prJpg pr)
    (prGenre pr)
    (prSchool pr)
    (prTimeframe pr)
    (optimisticDecodeConcepts (prConcepts pr))

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
      StillLife    -> toField ("still-ife" :: String)
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

-- Otherwise endpoint returns e.g. "German" instead of "german"
instance ToJSON School where
  toJSON a =
    case a of
      German        -> "german"
      Italian       -> "italian"
      Danish        -> "danish"
      Flemish       -> "flemish"
      Dutch         -> "dutch"
      Netherlandish -> "netherlandish"
      Swiss         -> "swiss"
      Other         -> "other"
      Russian       -> "russian"
      English       -> "english"
      Austrian      -> "austrian"
      Scottish      -> "scottish"
      Bohemian      -> "bohemian"
      French        -> "french"
      Spanish       -> "spanish"
      Belgian       -> "belgian"
      Hungarian     -> "hungarian"
      American      -> "american"
      Polish        -> "polish"
      Norwegian     -> "norwegian"
      Swedish       -> "swedish"
      Irish         -> "irish"
      Finnish       -> "finnish"
      Portuguese    -> "portuguese"
      Greek         -> "greek"
      Catalan       -> "catalan"

instance FromJSON School where
  parseJSON (String "german")        = return German
  parseJSON (String "italian")       = return Italian
  parseJSON (String "danish")        = return Danish
  parseJSON (String "flemish")       = return Flemish
  parseJSON (String "dutch")         = return Dutch
  parseJSON (String "netherlandish") = return Netherlandish
  parseJSON (String "swiss")         = return Swiss
  parseJSON (String "other")         = return Other
  parseJSON (String "russian")       = return Russian
  parseJSON (String "english")       = return English
  parseJSON (String "austrian")      = return Austrian
  parseJSON (String "scottish")      = return Scottish
  parseJSON (String "bohemian")      = return Bohemian
  parseJSON (String "french")        = return French
  parseJSON (String "spanish")       = return Spanish
  parseJSON (String "belgian")       = return Belgian
  parseJSON (String "hungarian")     = return Hungarian
  parseJSON (String "american")      = return American
  parseJSON (String "polish")        = return Polish
  parseJSON (String "norwegian")     = return Norwegian
  parseJSON (String "swedish")       = return Swedish
  parseJSON (String "irish")         = return Irish
  parseJSON (String "finnish")       = return Finnish
  parseJSON (String "portuguese")    = return Portuguese
  parseJSON (String "greek")         = return Greek
  parseJSON (String "catalan")       = return Catalan
  parseJSON _                        = error "Did not recognize the School"

instance FromField School where
  fromField f mdata = return school
    where
      school =
        case mdata of
          Just "german"        -> German
          Just "italian"       -> Italian
          Just "danish"        -> Danish
          Just "flemish"       -> Flemish
          Just "dutch"         -> Dutch
          Just "netherlandish" -> Netherlandish
          Just "swiss"         -> Swiss
          Just "other"         -> Other
          Just "russian"       -> Russian
          Just "english"       -> English
          Just "austrian"      -> Austrian
          Just "scottish"      -> Scottish
          Just "bohemian"      -> Bohemian
          Just "french"        -> French
          Just "spanish"       -> Spanish
          Just "belgian"       -> Belgian
          Just "hungarian"     -> Hungarian
          Just "american"      -> American
          Just "polish"        -> Polish
          Just "norwegian"     -> Norwegian
          Just "swedish"       -> Swedish
          Just "irish"         -> Irish
          Just "finnish"       -> Finnish
          Just "portuguese"    -> Portuguese
          Just "greek"         -> Greek
          Just "catalan"       -> Catalan
          _                    -> Other

instance ToField School where
  toField mdata =
    case mdata of
      German        -> toField ("german" :: String)
      Italian       -> toField ("italian" :: String)
      Danish        -> toField ("danish" :: String)
      Flemish       -> toField ("flemish" :: String)
      Dutch         -> toField ("dutch" :: String)
      Netherlandish -> toField ("netherlandish" :: String)
      Swiss         -> toField ("swiss" :: String)
      Other         -> toField ("other" :: String)
      Russian       -> toField ("russian" :: String)
      English       -> toField ("english" :: String)
      Austrian      -> toField ("austrian" :: String)
      Scottish      -> toField ("scottish" :: String)
      Bohemian      -> toField ("bohemian" :: String)
      French        -> toField ("french" :: String)
      Spanish       -> toField ("spanish" :: String)
      Belgian       -> toField ("belgian" :: String)
      Hungarian     -> toField ("hungarian" :: String)
      American      -> toField ("american" :: String)
      Polish        -> toField ("polish" :: String)
      Norwegian     -> toField ("norwegian" :: String)
      Swedish       -> toField ("swedish" :: String)
      Irish         -> toField ("irish" :: String)
      Finnish       -> toField ("finnish" :: String)
      Portuguese    -> toField ("portuguese" :: String)
      Greek         -> toField ("greek" :: String)
      Catalan       -> toField ("catalan" :: String)
      _             -> toField ("other" :: String)
