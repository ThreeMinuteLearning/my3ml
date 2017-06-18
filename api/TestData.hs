{-# LANGUAGE OverloadedStrings #-}

module TestData where

import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Prelude hiding (id)

import Api.Types (Account(Account), Story(..), Student(Student), Class(Class), School(School), admin, editor, teacher, student, teacher)
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


users :: [(Text, Account)]
users =
    [ ("admin", Account "aid" "admin" "admin" admin)
    , ("editor", Account "eid" "editor" "editor" editor)
    , ("teacher", Account "tid1" "teacher" "teacher" teacher)
    , ("mammy", Account "tid2" "mammy" "mammy" teacher)
    , ("jerry", Account "uid1" "jerry" "jerry" student)
    , ("tom", Account "uid2" "tom" "tom" student)
    , ("jack", Account "uid3" "jack" "jack" student)
    ]


schools_ :: [School]
schools_ =
    [ School "3" "T&J Academy" Nothing
    , School "4" "Pirate School" Nothing
    ]

classes_ :: [Class]
classes_ =
    [ Class "1" "A1" Nothing "3" "tid2" []
    , Class "2" "A2" Nothing "3" "tid2" []
    , Class "3" "B1" Nothing "3" "tid2" []
    , Class "4" "P1" Nothing "4" "tid2" []
    , Class "5" "P2" Nothing "4" "tid2" []
    ]

students_ :: [Student]
students_ =
    [ Student "1" "Jerry Mouse" Nothing 5 "3" False Nothing
    , Student "2" "Tom Cat" Nothing 3 "3" False Nothing
    , Student "3" "Butch" Nothing 3 "3" False Nothing
    , Student "4" "Nibbles" Nothing 2 "3" False Nothing
    , Student "5" "Tyke" Nothing 2 "3" False Nothing
    , Student "6" "Jack Sparrow" Nothing 8 "4" False Nothing
    ]
