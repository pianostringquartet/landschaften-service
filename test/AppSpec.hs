{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

--import           Control.Exception (throwIO)
--import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
--import           Network.HTTP.Types
--import           Network.Wai (Application)
--import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client

import           App
import           Test.Hspec

import qualified BuildQuery                       as BQ
import           Database.PostgreSQL.Simple.Types

conceptConstraint :: BQ.Constraint
conceptConstraint = BQ.Constraint "name" ["person", "saint"]

paintingConstraint :: BQ.Constraint
paintingConstraint = BQ.Constraint "school" ["German"]

-- also test that we return schools as lowercase?
-- cljs app and db expect lowercase -- who should enforce that?
spec :: Spec
spec =
  describe "buildQuery" $ do
    it "builds a query with painting and concept constraints" $
      BQ.buildQuery [conceptConstraint, paintingConstraint] `shouldBe`
      ( "select distinct t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t, paintings_concepts t2 where t.id = t2.painting_id and t2.name in ? and t.school in ?"
      , [In ["person", "saint"], In ["German"]])
    it "builds a query with only painting constaints" $
      BQ.buildQuery [paintingConstraint] `shouldBe`
      ( "select distinct t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t where t.school in ?"
      , [In ["German"]])
    it "builds a query with only concept constraints" $
      BQ.buildQuery [conceptConstraint] `shouldBe`
      ( "select distinct t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t, paintings_concepts t2 where t.id = t2.painting_id and t2.name in ?"
      , [In ["person", "saint"]])
    it "builds a query when no constraints" $
      BQ.buildQuery [] `shouldBe`
      ("select distinct author, title, date, wga_jpg, type, school, timeframe, concepts from paintings", [])
