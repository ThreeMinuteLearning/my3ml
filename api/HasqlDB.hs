{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module HasqlDB where

import           Control.Exception.Safe
import           Control.Monad (when, replicateM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import           Data.Functor.Contravariant
import           Data.Function (on)
import           Data.Int (Int64)
import           Data.List (foldl', groupBy)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.UUID as UUID
import           Hasql.Query (Query)
import           Hasql.Pool (Pool, use)
import qualified Hasql.Query as Q
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import qualified Hasql.Session as S
import           Prelude hiding (id, words)

import Api.Types
import DB

newtype HasqlDB = H Pool

instance DB HasqlDB where
    getStories db = runQuery selectAllStories db ()

    getStory = runQuery selectStoryById

    createStory = runQuery insertStory

    getSchools db = runQuery selectAllSchools db ()

    getSchool = runQuery selectSchoolById

    getTrails = runQuery selectTrailsBySchoolId

    createTrail = runQuery insertTrail

    deleteTrail db trailId_ = do
        nRows <- runQuery deleteTrailById db trailId_
        when (nRows == 0) $ liftIO $ throwString "Trail not found to delete"

    getClasses = runQuery selectClassesBySchool

    getClass = runQuery selectClassById

    getStudents = runQuery selectStudentsBySchool

    getStudent db schoolId_ studentId_ = runQuery selectStudentById  db (studentId_, schoolId_)

    createStudent = runQuery insertStudent

    getDictionary db = do
        elts <- groupBy ((==) `on` fst) <$> runQuery selectDictionary db ()
        return $ Map.fromList (map dictWord elts)

    lookupWord = runQuery selectWord


dictWord :: [(Text, b)] -> (Text, [b])
dictWord ((w, meaning):ws) = (w, meaning : map snd ws)
dictWord [] = ("", []) -- shouldn't happen


runQuery :: MonadIO m => Query p a -> HasqlDB -> p -> m a
runQuery q (H pool) p = liftIO $ do
    result <- use pool (S.query p q)
    case result of
      Left e -> liftIO $ throwString (show e)
      Right r -> return r


dvText :: D.Row Text
dvText = D.value D.text

evText :: E.Params Text
evText = E.value E.text

dvUUID :: D.Row Text
dvUUID = UUID.toText <$> D.value D.uuid

dArray :: D.Value a -> D.Row [a]
dArray v = D.value (D.array (D.arrayDimension replicateM (D.arrayValue v)))

eArray :: Foldable t => E.Value b -> E.Params (t b)
eArray v = E.value (E.array (E.arrayDimension foldl' (E.arrayValue v)))


dictEntryValue :: D.Value DictEntry
dictEntryValue = D.composite (DictEntry <$> D.compositeValue D.text <*> (fromIntegral <$> D.compositeValue D.int2))

dictEntryTupleValue :: D.Value (Text, Int)
dictEntryTupleValue = D.composite ((,) <$> D.compositeValue D.text <*> (fromIntegral <$> D.compositeValue D.int2))

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

selectStorySql :: ByteString
selectStorySql = "SELECT id, title, img_url, level, curriculum, tags, content, words, created_at FROM story"

selectAllStories :: Query () [Story]
selectAllStories =
    Q.statement selectStorySql mempty (D.rowsList storyRow) True

selectStoryById :: Query StoryId (Maybe Story)
selectStoryById =
    Q.statement (selectStorySql <> " WHERE id = $1") evText (D.maybeRow storyRow) True

storyRow :: D.Row Story
storyRow = Story
    <$> dvText
    <*> dvText
    <*> dvText
    <*> (fromIntegral <$> D.value D.int2)
    <*> dvText
    <*> dArray D.text
    <*> dvText
    <*> dArray dictEntryValue
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
storyEncoder = contramap (id :: Story -> Text) evText
    <> contramap title evText
    <> contramap img evText
    <> contramap (fromIntegral . storyLevel) (E.value E.int4)
    <> contramap curriculum evText
    <> contramap tags (eArray E.text)
    <> contramap content evText
    <> contramap (map word . words) (eArray E.text)
    <> contramap (map (fromIntegral . index) . words) (eArray E.int2)
    <> contramap date (E.value E.timestamptz)
  where
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

selectStudentSql :: ByteString
selectStudentSql = "SELECT id, name, description, level, sub, school_id FROM student"

selectStudentsBySchool :: Query SchoolId [Student]
selectStudentsBySchool = Q.statement sql evText (D.rowsList studentRow) True
  where
    sql = selectStudentSql <> " WHERE school_id = $1 :: uuid"

selectStudentById :: Query (StudentId, SchoolId) (Maybe Student)
selectStudentById = Q.statement sql encode (D.maybeRow studentRow) True
  where
    sql = selectStudentSql <> " WHERE id = $1 :: uuid AND school_id = $2 :: uuid"
    encode = contramap fst evText <> contramap snd evText

studentRow :: D.Row Student
studentRow = Student
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


-- Schools

selectSchoolSql :: ByteString
selectSchoolSql = "SELECT id, name, description FROM school"

selectAllSchools :: Query () [School]
selectAllSchools =
    Q.statement selectSchoolSql mempty (D.rowsList schoolRow) True

selectSchoolById :: Query SchoolId (Maybe School)
selectSchoolById =
    Q.statement (selectSchoolSql <> " WHERE id = $1 :: uuid") evText (D.maybeRow schoolRow) True

schoolRow :: D.Row School
schoolRow = School
    <$> dvUUID
    <*> dvText
    <*> D.nullableValue D.text


-- Classes

selectClassSql :: ByteString
selectClassSql = "SELECT id, name, description, school_id FROM class"

selectClassesBySchool :: Query SchoolId [Class]
selectClassesBySchool = Q.statement sql evText (D.rowsList classRow) True
  where
    sql = selectClassSql <> " WHERE school_id = $1 :: uuid"

selectClassById :: Query ClassId (Maybe Class)
selectClassById = Q.statement sql evText (D.maybeRow classRow) True
  where
    sql = selectClassSql <> " WHERE id = $1 :: uuid"

classRow :: D.Row Class
classRow = Class
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


-- Trails

selectTrailsBySchoolId :: Query SchoolId [StoryTrail]
selectTrailsBySchoolId = Q.statement sql evText (D.rowsList trailRow) True
  where
    sql = "SELECT id, name, school_id, stories FROM trail WHERE school_id = $1"

trailRow :: D.Row StoryTrail
trailRow = StoryTrail
    <$> dvUUID
    <*> dvText
    <*> dvUUID
    <*> dArray D.text

insertTrail :: Query StoryTrail ()
insertTrail = Q.statement sql encode D.unit True
  where
    sql = "INSERT INTO trail (id, name, school_id, stories) \
              \ VALUES ($1, $2, $3, $4)"
    encode = contramap (id :: StoryTrail -> TrailId) evText
        <> contramap (name :: StoryTrail -> Text) evText
        <> contramap (schoolId :: StoryTrail -> Text) evText
        <> contramap (stories :: StoryTrail -> [StoryId]) (eArray E.text)

deleteTrailById :: Query TrailId Int64
deleteTrailById = Q.statement sql evText D.rowsAffected True
  where
    sql = "DELETE FROM trail WHERE id = $1 :: uuid"


-- Word dictionary

selectDictionary :: Query () [(Text, WordDefinition)]
selectDictionary = Q.statement sql mempty decode True
  where
    sql = "SELECT word, index, definition, uses_words FROM dict ORDER BY word, index"
    decode = D.rowsList $ do
        word_ <- dvText
        _ <- D.value D.int2
        defn <- dvText
        uses <- dArray dictEntryTupleValue
        return (word_, (defn, uses))

selectWord :: Query Text [WordDefinition]
selectWord = Q.statement sql evText decode True
  where
    sql = "SELECT index, definition, uses_words \
          \ FROM dict \
          \ WHERE word = $1 \
          \ ORDER BY index"
    decode = D.rowsList $ D.value D.int2 >> (,) <$> dvText <*> dArray dictEntryTupleValue
