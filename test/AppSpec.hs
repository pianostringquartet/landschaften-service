{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import           App
import           Servant
import           Servant.Client
import           Test.Hspec

import qualified BuildQuery                       as BQ
import           Database.PostgreSQL.Simple.Types
import qualified Domain                           as D
import qualified Handlers                         as H

conceptConstraint :: BQ.Constraint
conceptConstraint = BQ.Constraint "name" ["person", "saint"]

paintingConstraint :: BQ.Constraint
paintingConstraint = BQ.Constraint "school" ["German"]

painting1 :: D.Painting
painting1 =
  D.Painting
    0
    "Picasso"
    "Guernica"
    "1930s"
    ""
    D.Historical
    D.Spanish
    ""
    [D.Concept "war" 0.82, D.Concept "art" 0.95, D.Concept "abstract" 0.90, D.Concept "cubism" 0.62]

painting2 :: D.Painting
painting2 =
  D.Painting
    1
    "Picasso"
    "Demoiselles D'Avignon"
    "1920s"
    ""
    D.Interior
    D.Spanish
    ""
    [D.Concept "female" 0.96, D.Concept "art" 0.97, D.Concept "abstract" 0.93, D.Concept "cubism" 0.79]

painting3 :: D.Painting
painting3 =
  D.Painting
    2
    "Pollock"
    "Autumn Rhythm (Number 30)"
    "1950s"
    ""
    D.OtherGenre
    D.American
    ""
    [D.Concept "expressionism" 0.96, D.Concept "art" 0.94, D.Concept "abstract" 0.98, D.Concept "cubism" 0.67]

painting4 :: D.Painting
painting4 =
  D.Painting
    3
    "Pollock"
    "Number 1 (Lavender Mist)"
    "1950s"
    ""
    D.OtherGenre
    D.American
    ""
    [D.Concept "expressionism" 0.98, D.Concept "art" 0.98, D.Concept "abstract" 0.97, D.Concept "cubism" 0.54]

paintings :: [D.Painting]
paintings = [painting1, painting2, painting3, painting4]

spec :: Spec
spec =
  describe "Query and handler tests" $ do
    describe "buildQuery" $ do
      it "builds a query with painting and concept constraints" $
        BQ.buildQuery [conceptConstraint, paintingConstraint] `shouldBe`
        ( "select distinct t.id, t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t, paintings_concepts t2 where t.id = t2.painting_id and t2.name in ? and t2.value >= 0.85 and t.school in ?"
        , [In ["person", "saint"], In ["German"]])
      it "builds a query with only painting constaints" $
        BQ.buildQuery [paintingConstraint] `shouldBe`
        ( "select distinct t.id, t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t where t.school in ?"
        , [In ["German"]])
      it "builds a query with only concept constraints" $
        BQ.buildQuery [conceptConstraint] `shouldBe`
        ( "select distinct t.id, t.author, t.title, t.date, t.wga_jpg, t.type, t.school, t.timeframe, t.concepts from paintings t, paintings_concepts t2 where t.id = t2.painting_id and t2.name in ? and t2.value >= 0.85"
        , [In ["person", "saint"]])
      it "builds a query when no constraints" $
        BQ.buildQuery [] `shouldBe`
        ("select distinct id, author, title, date, wga_jpg, type, school, timeframe, concepts from paintings", [])
    describe "getSampleSize" $ it "retrieves samples size" $ H.getSampleSize 30000 `shouldBe` 379
    describe "paintingsToPaintingsResponse" $ do
      it "turns paintings into a paintings response" $
        H.paintingsToPaintingsResponse 200 50 paintings `shouldBe`
        H.PaintingsResponse
          paintings
          [0, 1, 2, 3]
          [("abstract", 1.0), ("art", 1.0), ("cubism", 1.0), ("expressionism", 0.5), ("female", 0.25), ("war", 0.25)]
      it "does not send more paintings or concept-frequencies than requested" $
        H.paintingsToPaintingsResponse 3 2 paintings `shouldBe`
        H.PaintingsResponse [painting1, painting2, painting3] [0, 1, 2, 3] [("abstract", 1.0), ("art", 1.0)]
