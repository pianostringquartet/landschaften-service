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

data Constraint =
  Constraint
    { column :: String
    , values :: [String]
    }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Constraint

instance FromJSON Constraint

newtype ConstraintsInfo =
  ConstraintsInfo
    { constraints :: [Constraint]
    }
  deriving (Eq, Show, Read, Generic)

instance ToJSON ConstraintsInfo

instance FromJSON ConstraintsInfo

--getConstraintsFromJson :: a -> [Constraint]
--getConstraintsFromJson = undefined

isConceptConstraint :: Constraint -> Bool
isConceptConstraint constraint = column constraint == "name"

isPaintingConstraint :: Constraint -> Bool
isPaintingConstraint constraint = column constraint `elem` ["school", "timeframe", "type"]

type ParameterizedQuery = (Query, [In [String]])

--noConstraintsBase :: String
noConstraintsBase = "select distinct author, title, wga_jpg, type, school, timeframe, concepts from paintings"

base :: [Constraint] -> String
base cs
  | hasConceptConstraints =
    "select distinct t.author, t.title, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t, paintings_concepts t2 where t.id = t2.painting_id and "
  | hasPaintingConstraints && not hasConceptConstraints =
    "select distinct t.author, t.title, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t where "
  | otherwise = noConstraintsBase -- "select distinct author, title, wga_jpg, type, school, timeframe, concepts from paintings"
  where
    hasConceptConstraints = any isConceptConstraint cs
    hasPaintingConstraints = any isPaintingConstraint cs

type Snippet = (String, In [String])

conceptSnippet :: Constraint -> Snippet
conceptSnippet c = ("t2." ++ column c ++ " in ?", In $ values c)

paintingSnippet :: Constraint -> Snippet
paintingSnippet c = ("t." ++ column c ++ " in ?", In $ values c)

buildQuery :: [Constraint] -> ParameterizedQuery
buildQuery cs = (Query $ fromString queryString, queryParams)
  where
    allSnippets =
      map
        (\c ->
           if isPaintingConstraint c
             then paintingSnippet c
             else conceptSnippet c)
        cs
    queryString = base cs ++ intercalate " and " (map fst allSnippets)
    queryParams = map snd allSnippets

c1 :: Constraint
c1 = Constraint "name" ["person", "saint"]

c2 :: Constraint
c2 = Constraint "school" ["German"]

cs :: [Constraint]
cs = [c1, c2]

--snippets :: [Snippet]
--snippets = [conceptSnippet c1, paintingSnippet c2]
--
--allSnippets :: [Snippet]
--allSnippets = map (\c -> if isPaintingConstraint c then paintingSnippet c else conceptSnippet c) cs
--
--queryString :: String
--queryString = intercalate " and " $ map fst allSnippets
--
--queryParams :: [In [String]]
--queryParams = map snd allSnippets
sampleQuery :: ParameterizedQuery
sampleQuery = buildQuery cs
-- :set -XOverloadedStrings
