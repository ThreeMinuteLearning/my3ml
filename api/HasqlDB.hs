{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module HasqlDB where

import           Control.Monad (replicateM)
import           Data.Functor.Contravariant
import           Data.List (foldl')
import           Data.Maybe (fromJust)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.UUID as UUID
import           Data.Vector (Vector)
import           Hasql.Query (Query)
import qualified Hasql.Query as Q
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import           Prelude hiding (id, words)

import Api.Types

dvText :: D.Row Text
dvText = D.value D.text

evText :: E.Params Text
evText = E.value E.text

dvUUID :: D.Row Text
dvUUID = UUID.toText <$> D.value D.uuid

-- User accounts

insertAccount :: Query Account ()
insertAccount = Q.statement sql encode D.unit True
  where
    sql = "INSERT INTO login (id, username, password, user_type) VALUES ($1 :: uuid, $2, $3, $4 :: user_type)"
    encode = contramap (id :: Account -> Text) evText
        <> contramap (username :: Account -> Text) evText
        <> contramap (password :: Account -> Text) evText
        <> contramap (userType . (role :: Account -> UserType)) evText

-- Stories

selectAllStories :: Query () (Vector Story)
selectAllStories =
    Q.statement sql mempty (D.rowsVector decoder) True
  where
    sql = "SELECT id, title, img_url, level, curriculum, tags, content, words, created_at FROM story"

    dictEntryValue = D.composite (DictEntry <$> D.compositeValue D.text <*> (fromIntegral <$> D.compositeValue D.int2))
    array v = D.value (D.array (D.arrayDimension replicateM (D.arrayValue v)))

    decoder = Story
        <$> (Just <$> dvText)
        <*> dvText
        <*> dvText
        <*> (fromIntegral <$> D.value D.int2)
        <*> dvText
        <*> array D.text
        <*> dvText
        <*> array dictEntryValue
        <*> D.value D.timestamptz

insertStory :: Query Story ()
insertStory = Q.statement sql storyEncoder D.unit True
  where
    sql = "INSERT INTO story (id, title, img_url, level, curriculum, tags, content, words, created_at) \
                 \VALUES ($1, $2, $3, $4, $5, $6, $7, (array(select word::dict_entry from unnest ($8, $9) as word)), $10)"

updateStory :: Query Story ()
updateStory = Q.statement sql storyEncoder D.unit True
  where
    sql = "UPDATE story SET title=$2, img_url=$3, level=$4, curriculum=$5, tags=$6, content=$7, words=(array(select word::dict_entry from unnest ($8, $9) as word)) WHERE id=$1"

storyEncoder :: E.Params Story
storyEncoder = contramap (fromJust . storyId) evText
    <> contramap title evText
    <> contramap img evText
    <> contramap (fromIntegral . storyLevel) (E.value E.int4)
    <> contramap curriculum evText
    <> contramap tags (array E.text)
    <> contramap content evText
    <> contramap (map word . words) (array E.text)
    <> contramap (map (fromIntegral . index) . words) (array E.int2)
    <> contramap date (E.value E.timestamptz)
  where
    array v = E.value (E.array (E.arrayDimension foldl' (E.arrayValue v)))
    storyId = id :: Story -> Maybe Text
    storyLevel = level :: Story -> Int

-- School

insertSchool :: Query School ()
insertSchool = Q.statement sql encode D.unit True
  where
    sql = "INSERT INTO school (id, name, description) values ($1 :: uuid,$2,$3)"
    encode = contramap (id :: School -> SchoolId) evText
        <> contramap (name :: School -> Text) evText
        <> contramap (description :: School -> Maybe Text) (E.nullableValue E.text)

-- Students

selectStudentsBySchool :: Query SchoolId (Vector Student)
selectStudentsBySchool = Q.statement sql encode (D.rowsVector decode) True
  where
    sql = "SELECT id, name, description, level, sub, school_id FROM student where school_id=$1 :: uuid"
    encode = evText
    decode = Student
        <$> dvUUID
        <*> dvText
        <*> D.nullableValue D.text
        <*> (fromIntegral <$> D.value D.int2)
        <*> dvUUID
        <*> dvUUID

insertStudent :: Query Student ()
insertStudent = Q.statement sql encode D.unit True
  where
    sql = "INSERT INTO student (id, name, description, level, sub, school_id) values ($1 :: uuid, $2, $3, $4, $5 :: uuid, $6 :: uuid)"
    encode = contramap (id :: Student -> Text) evText
        <> contramap (name :: Student -> Text) evText
        <> contramap (description :: Student -> Maybe Text) (E.nullableValue E.text)
        <> contramap (fromIntegral . (level :: Student -> Int)) (E.value E.int4)
        <> contramap (accountId :: Student -> SubjectId) evText
        <> contramap (schoolId :: Student -> SchoolId) evText

-- Classes

selectClassesBySchool :: Query SchoolId (Vector Class)
selectClassesBySchool = Q.statement sql encode (D.rowsVector decode) True
  where
    sql = "SELECT id, name, description, school_id FROM class where school_id=$1 :: uuid"
    encode = evText
    decode = Class
        <$> dvUUID
        <*> dvText
        <*> D.nullableValue D.text
        <*> dvUUID
        <*> pure []

insertClass :: Query Class ()
insertClass = Q.statement sql encode D.unit True
  where
    sql = "INSERT INTO class (id, name, description, school_id) values ($1 :: uuid, $2, $3, $4 :: uuid)"
    encode = contramap (id :: Class -> ClassId) evText
        <> contramap (name :: Class -> Text) evText
        <> contramap (description :: Class -> Maybe Text) (E.nullableValue E.text)
        <> contramap (schoolId :: Class -> SchoolId) evText
