{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module BuildQuery where

import           Data.Aeson
import           Data.ByteString.UTF8             (fromString)
import           Data.List
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
import           GHC.Generics

data Constraint =
  Constraint
    { column :: String
    , values :: [String]
    }
  deriving (Eq, Show, Read, Generic)

instance FromJSON Constraint

newtype ConstraintsInfo =
  ConstraintsInfo
    { constraints :: [Constraint]
    }
  deriving (Eq, Show, Read, Generic)

instance FromJSON ConstraintsInfo

-- TODO: Can you instead parse the constraints-json and create a subtype? e.g.
-- Constraints = PaintingConstraints | ConceptConstraints

-- ... better?: put them in 'mutually exclusive' vs. 'inclusive / can overlap' sets?
-- Constraints = AndConstraint | OrConstraint

isConceptConstraint :: Constraint -> Bool
isConceptConstraint constraint = column constraint == "name"

isPaintingConstraint :: Constraint -> Bool
isPaintingConstraint constraint = column constraint `elem` 
                                        ["school", "timeframe", "type", "author"]

-- better?: just use 'select distinct author from paintings'
-- this returns 1 row per painting, and then we pull out just the author name
-- hence the duplicate author names
namesQuery :: Query
--namesQuery = "select distinct author, title, date, wga_jpg, type, school, timeframe, concepts from paintings"
namesQuery = "select distinct author from paintings"

conceptsQuery :: Query
conceptsQuery = "select distinct name from paintings_concepts"

noConstraintsQuery :: Query
noConstraintsQuery = "select distinct id, author, title, date, wga_jpg, type, school, timeframe, concepts from paintings"

type ParameterizedQuery = (Query, [In [String]])

-- better?: match on conceptConstraint vs. paintingConstraint
base :: [Constraint] -> String
base cs
  | hasConceptConstraints =
     "select distinct t.id, t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t, paintings_concepts t2 where t.id = t2.painting_id and "
  | hasPaintingConstraints && not hasConceptConstraints =
    "select distinct t.id, t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t where "
  where
    hasConceptConstraints = any isConceptConstraint cs
    hasPaintingConstraints = any isPaintingConstraint cs

-- TODO: parameterize rather than hardcode the minimum concept certainty
buildQuery :: [Constraint] -> ParameterizedQuery
buildQuery cs =
  if null cs
    then (noConstraintsQuery, [])
    else (Query $ fromString queryString, queryParams)
  where
    paintingSnippet c = ("t." ++ column c ++ " in ?", In $ values c)
--    conceptSnippet c = ("t2." ++ column c ++ " in ?", In $ values c)

    conceptSnippet c = ("t2." ++ column c ++ " in ? and t2.value >= 0.85", In $ values c)
    allSnippets =
      map
        (\c ->
           if isPaintingConstraint c
             then paintingSnippet c
             else conceptSnippet c)
        cs
    queryString = base cs ++ intercalate " and " (map fst allSnippets)
    queryParams = map snd allSnippets