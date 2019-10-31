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


-- then pattern match on these values instead of using the isPaintingConstraint fn etc.
-- make these changes, then tests should work, then handler should work

-- but this enum

 -- don't make a separate type / enum here;
 -- just make a type for Timeframe that is used by Painting and Constraint;
 -- may be tricky in case of `Name`
--data ConstraintType = Name | School | Timeframe | Genre | Author
--  deriving (Eq, Show, Read, Generic)

-- now update the functions? or the example constraints?

--instance ToJSON ConstraintType

-- need a field modifier for the names, right?
--instance FromJSON ConstraintType

--
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

namesQuery :: Query
namesQuery = "select distinct author, title, date, wga_jpg, type, school, timeframe, concepts from paintings"

conceptsQuery :: Query
conceptsQuery = "select distinct name from paintings_concepts"

noConstraintsQuery :: Query
noConstraintsQuery = "select distinct author, title, date, wga_jpg, type, school, timeframe, concepts from paintings"

type ParameterizedQuery = (Query, [In [String]])


paintingsTableAlias = "t"
conceptTableAlias = "t2"

-- build by intercalating t and ", " with the column name list
 
-- not really that helpful, especially given that you have tests for this part? 
--paintingsTableColumns = paintingsTableAlias ++ ".author, "
--                        ++ paintingsTableAlias ++ ".title, "
--                        ++ paintingsTableAlias ++ ".date, "
--                        ++ paintingsTableAlias ++ ".wga_jpg, "
--                        ++ paintingsTableAlias ++ ".type, "
--                        ++ paintingsTableAlias ++ ".school, "
--                        ++ paintingsTableAlias ++ ".timeframe, "
--                        ++ paintingsTableAlias ++ ".concepts"

--paintingsTableColumns = intercalate ", " (intercalate (paintingsTableAlias ++ ".") ["author", "title", "date", "wga_jpg", "type", "school", "timeframe", "concepts"])

-- better to match on conceptConstraint vs. paintingConstraint
base :: [Constraint] -> String
base cs
  | hasConceptConstraints =
     "select distinct t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t, paintings_concepts t2 where t.id = t2.painting_id and "
--    "select distinct " ++ paintingsTableColumns ++ " from paintings " ++ paintingsTableAlias ++ ", paintings_concepts " ++ conceptTableAlias ++ " where t.id = t2.painting_id and "
  | hasPaintingConstraints && not hasConceptConstraints =
    "select distinct t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t where "
--  | otherwise = noConstraintsQuery
  where
    hasConceptConstraints = any isConceptConstraint cs
    hasPaintingConstraints = any isPaintingConstraint cs


-- this should actually first check if there are constraints or not; if there aren't, then return noConstraintsQuery right away
-- how were you returning a parameterized query when there were no constraints
buildQuery :: [Constraint] -> ParameterizedQuery
--buildQuery cs = (Query $ fromString queryString, queryParams)
buildQuery cs =
  if null cs
    then (noConstraintsQuery, [])
    else (Query $ fromString queryString, queryParams)
  where
    paintingSnippet c = (paintingsTableAlias ++ "." ++ column c ++ " in ?", In $ values c)
    conceptSnippet c = (conceptTableAlias ++ "." ++ column c ++ " in ?", In $ values c)
    allSnippets =
      map
        (\c ->
           if isPaintingConstraint c
             then paintingSnippet c
             else conceptSnippet c)
        cs
    queryString = base cs ++ intercalate " and " (map fst allSnippets)
    queryParams = map snd allSnippets