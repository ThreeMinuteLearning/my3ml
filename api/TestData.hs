{-# LANGUAGE OverloadedStrings    #-}

module TestData where

import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import           Prelude hiding (id)

import Api.Types (Story(..), Student(Student), Class(Class), School(School))
import DB

mkTestDB :: IO AtomicDB
mkTestDB = do
    storyFile <- B.readFile "./data/allstories.json"
    dictFile <- B.readFile "./data/dict.json"
    let stories' = case J.eitherDecodeStrict storyFile of
            Right s -> s
            Left e -> error $ "Failed to decode stories " ++ show e

        dict = case J.eitherDecodeStrict dictFile of
            Right d -> d
            Left e -> error $ "Failed to decode dictionary" ++ show e

        storyIds = map id stories'
        starterStories' = take 20 stories'

        db = InMemoryDB
            { stories = Map.fromList (zip storyIds stories')
            , sampleStories = starterStories'
            , dictionary = dict
            , trails = []
            , schools = schools_
            , classes = classes_
            , students = students_
            }
    mkDB db

schools_ :: [School]
schools_ =
    [ School "3" "T&J Academy" Nothing
    , School "4" "Pirate School" Nothing
    ]

classes_ :: [Class]
classes_ =
    [ Class "1" "A1" Nothing "3" []
    , Class "2" "A2" Nothing "3" []
    , Class "3" "B1" Nothing "3" []
    , Class "4" "P1" Nothing "4" []
    , Class "5" "P2" Nothing "4" []
    ]

students_ :: [Student]
students_ =
    [ Student "1" "Jerry Mouse" Nothing 5 "3"
    , Student "2" "Tom Cat" Nothing 3 "3"
    , Student "3" "Butch" Nothing 3 "3"
    , Student "4" "Nibbles" Nothing 2 "3"
    , Student "5" "Tyke" Nothing 2 "3"
    , Student "6" "Jack Sparrow" Nothing 8 "4"
    ]
