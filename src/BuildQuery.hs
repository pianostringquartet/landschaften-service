{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module BuildQuery where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.UTF8             (fromString)
import           Data.List
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
import           Domain
import           GHC.Generics

import Servant.API.ContentTypes (MimeUnrender) -- remove?



-- type-driven design: need enough types to rule out impossible states 


-- then pattern match on these values instead of using the isPaintingConstraint fn 
data ConstraintType = Name | School | Timeframe | Genre | Author 
  deriving (Eq, Show, Read, Generic) 

--instance ToJSON ConstraintType

-- need a field modifier for the names, right?
instance FromJSON ConstraintType 

data Constraint =
  Constraint
    { column :: String
--    { column :: ConstraintType
    , values :: [String]
    }
  deriving (Eq, Show, Read, Generic)

--instance ToJSON Constraint

instance FromJSON Constraint

newtype ConstraintsInfo =
  ConstraintsInfo
    { constraints :: [Constraint]
    }
  deriving (Eq, Show, Read, Generic)

--instance ToJSON ConstraintsInfo

instance FromJSON ConstraintsInfo

-- can you instead parse the constraints-json and create a subtype? e.g.
-- Constraints = PaintingConstraints | ConceptConstraints
-- better?:
-- Constraints = AndConstraint | OrConstraint
-- ... then can pattern match / define particular arities 

-- put them in 'mutually exclusive' vs. 'inclusive / can overlap' sets?
 
isConceptConstraint :: Constraint -> Bool
isConceptConstraint constraint = column constraint == "name"

isPaintingConstraint :: Constraint -> Bool
isPaintingConstraint constraint = column constraint `elem` 
                                        ["school", "timeframe", "type", "author"]

type ParameterizedQuery = (Query, [In [String]])


--noConstraintsBase :: String
noConstraintsBase = "select distinct author, title, date, wga_jpg, type, school, timeframe, concepts from paintings"

base :: [Constraint] -> String
base cs
  | hasConceptConstraints =
    "select distinct t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t, paintings_concepts t2 where t.id = t2.painting_id and "
  | hasPaintingConstraints && not hasConceptConstraints =
    "select distinct t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t where "
  | otherwise = noConstraintsBase
  where
    hasConceptConstraints = any isConceptConstraint cs
    hasPaintingConstraints = any isPaintingConstraint cs

buildQuery :: [Constraint] -> ParameterizedQuery
buildQuery cs = (Query $ fromString queryString, queryParams)
  where
    paintingSnippet c = ("t." ++ column c ++ " in ?", In $ values c)
    conceptSnippet c = ("t2." ++ column c ++ " in ?", In $ values c)
    allSnippets =
      map
        (\c -> if isPaintingConstraint c then paintingSnippet c else conceptSnippet c)
        cs
    queryString = base cs ++ intercalate " and " (map fst allSnippets)
    queryParams = map snd allSnippets


c1 :: Constraint
c1 = Constraint "name" ["person", "saint"]

c2 :: Constraint
c2 = Constraint "school" ["German"]

cs :: [Constraint]
cs = [c1, c2]

sampleQuery :: ParameterizedQuery
sampleQuery = buildQuery cs
