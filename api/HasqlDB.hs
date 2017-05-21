{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module HasqlDB where

import           Control.Exception.Safe
import           Control.Monad (when, replicateM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Functor.Contravariant
import           Data.Function (on)
import           Data.Int (Int64)
import           Data.List (foldl', groupBy)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import           Hasql.Query (Query)
import           Hasql.Pool (Pool, use, acquire)
import qualified Hasql.Query as Q
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import qualified Hasql.Session as S
import           Prelude hiding (id, words)

import Api.Types
import DB


mkDB :: String -> IO HasqlDB
mkDB connStr = H <$> acquire (10, 120, BC.pack connStr)

newtype HasqlDB = H Pool

instance DB HasqlDB where
    getAccountByUsername = runQuery selectAccountByUsername

    getStories = runQuery selectAllStories ()

    getStory = runQuery selectStoryById

    createStory = runQuery insertStory

    getSchools = runQuery selectAllSchools ()

    getSchool = runQuery selectSchoolById

    getTrails = runQuery selectTrailsBySchoolId

    createTrail = runQuery insertTrail

    deleteTrail db trailId_ = do
        nRows <- runQuery deleteTrailById db trailId_
        when (nRows == 0) $ liftIO $ throwString "Trail not found to delete"

    getClasses = runQuery selectClassesBySchool

    getClass = runQuery selectClassById

    createClass = runQuery insertClass

    getStudents = runQuery selectStudentsBySchool

    getStudent schoolId_ studentId_ = runQuery selectStudentById (studentId_, schoolId_)

    getStudentBySubjectId = runQuery selectStudentBySubjectId

    createStudent (nm, lvl, schoolId_) creds db = runSession db $ do
        subId <- S.query creds insertStudentAccount
        let s = Student (UUID.toText subId) nm Nothing lvl schoolId_
        S.query (s, subId) insertStudent
        return s

    getTeacherBySubjectId = runQuery selectTeacherBySubjectId

    getDictionary db = do
        elts <- groupBy ((==) `on` fst) <$> runQuery selectDictionary () db
        return $ Map.fromList (map dictWord elts)

    lookupWord = runQuery selectWord

    getAnswers schoolId_ studentId_ storyId_ db = case (studentId_, storyId_) of
        (Just sid, _) -> runQuery selectAnswersByStudent (sid, schoolId_) db
        (_, Just sid) -> runQuery selectAnswersByStory (schoolId_, sid) db
        _ -> runQuery selectAnswersBySchool schoolId_ db

    createAnswer = runQuery insertAnswer

dictWord :: [(Text, b)] -> (Text, [b])
dictWord ((w, meaning):ws) = (w, meaning : map snd ws)
dictWord [] = ("", []) -- shouldn't happen


runQuery :: MonadIO m => Query p a -> p -> HasqlDB -> m a
runQuery q p db = runSession db (S.query p q)

runSession :: MonadIO m => HasqlDB -> S.Session a -> m a
runSession (H pool) s = liftIO $ do
    result <- use pool s
    case result of
        Left e -> liftIO $ throwString (show e)
        Right r -> return r

dvText :: D.Row Text
dvText = D.value D.text

evText :: E.Params Text
evText = E.value E.text

eTextPair :: E.Params (Text, Text)
eTextPair = contramap fst evText <> contramap snd evText

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


userTypeValue :: D.Row UserType
userTypeValue = -- D.composite (uType <$> D.compositeValue D.text)
    uType <$> dvText
  where
    uType t = case T.unpack t of
        "Teacher" -> teacher
        "Admin" -> admin
        "Editor" -> editor
        _ -> student

-- User accounts

selectAccountByUsername :: Query Text (Maybe Account)
selectAccountByUsername = Q.statement sql evText (D.maybeRow decode) True
  where
    sql = "SELECT id, username, password, user_type :: text FROM login WHERE username = $1"
    decode = Account
        <$> dvUUID
        <*> dvText
        <*> dvText
        <*> userTypeValue

insertStudentAccount :: Query (Text, Text) UUID.UUID
insertStudentAccount = Q.statement sql eTextPair (D.singleRow (D.value D.uuid))True
  where
    sql = "INSERT INTO login (username, password) VALUES (lower($1), $2) RETURNING id"

-- Stories

selectStorySql :: ByteString
selectStorySql = "SELECT id, title, img_url, level, curriculum, tags, content, words, clarify_word, created_at FROM story"

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
    <*> dvText
    <*> D.value D.timestamptz

insertStory :: Query Story ()
insertStory = Q.statement sql storyEncoder D.unit True
  where
    sql = "INSERT INTO story (id, title, img_url, level, curriculum, tags, content, words, clarify_word) \
                 \VALUES ($1, $2, $3, $4, $5, $6, $7, (array(select word::dict_entry from unnest ($8, $9) as word)), $10)"

updateStory :: Query Story ()
updateStory = Q.statement sql storyEncoder D.unit True
  where
    sql = "UPDATE story SET title=$2, img_url=$3, level=$4, curriculum=$5, tags=$6, content=$7, words=(array(select word::dict_entry from unnest ($8, $9) as word), clarify_word=$10) WHERE id=$1"

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
    <> contramap clarifyWord evText
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

-- Teachers

selectTeacherSql :: ByteString
selectTeacherSql = "SELECT id, name, bio, school_id FROM teacher"

selectTeacherBySubjectId :: Query SubjectId Teacher
selectTeacherBySubjectId = Q.statement sql evText (D.singleRow teacherRow) True
  where
    sql = selectTeacherSql <> " WHERE id = $1 :: uuid"

teacherRow :: D.Row Teacher
teacherRow = Teacher
    <$> dvUUID
    <*> dvText
    <*> D.nullableValue D.text
    <*> dvUUID

-- Students

selectStudentSql :: ByteString
selectStudentSql = "SELECT id, name, description, level, school_id FROM student"

selectStudentsBySchool :: Query SchoolId [Student]
selectStudentsBySchool = Q.statement sql evText (D.rowsList studentRow) True
  where
    sql = selectStudentSql <> " WHERE school_id = $1 :: uuid"

selectStudentById :: Query (SubjectId, SchoolId) (Maybe Student)
selectStudentById = Q.statement sql eTextPair (D.maybeRow studentRow) True
  where
    sql = selectStudentSql <> " WHERE id = $1 :: uuid AND school_id = $2 :: uuid"

selectStudentBySubjectId :: Query SubjectId Student
selectStudentBySubjectId = Q.statement sql evText (D.singleRow studentRow) True
  where
    sql = selectStudentSql <> " WHERE id = $1 :: uuid"

studentRow :: D.Row Student
studentRow = Student
    <$> dvUUID
    <*> dvText
    <*> D.nullableValue D.text
    <*> (fromIntegral <$> D.value D.int2)
    <*> dvUUID

insertStudent :: Query (Student, UUID.UUID) ()
insertStudent = Q.statement sql encode D.unit True
  where
    sql = "INSERT INTO student (id, name, description, level, school_id) values ($1, $2, $3, $4, $5 :: uuid)"
    encode = contramap snd (E.value E.uuid)
        <> contramap ((name :: Student -> Text) . fst) evText
        <> contramap ((description :: Student -> Maybe Text) . fst) (E.nullableValue E.text)
        <> contramap ((fromIntegral . (level :: Student -> Int)) . fst) (E.value E.int4)
        <> contramap ((schoolId :: Student -> SchoolId) . fst) evText


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
selectClassSql = "SELECT id, name, description, school_id, created_by, array(SELECT student_id :: text FROM student_class WHERE class_id = class.id) AS students FROM class"

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
    <*> dvUUID
    <*> dArray D.text

insertClass :: Query Class ()
insertClass = Q.statement sql encode D.unit True
  where
    sql = "INSERT INTO class (id, name, description, school_id, created_by) values ($1 :: uuid, $2, $3, $4 :: uuid, $5 :: uuid)"
    encode = contramap (id :: Class -> ClassId) evText
        <> contramap (name :: Class -> Text) evText
        <> contramap (description :: Class -> Maybe Text) (E.nullableValue E.text)
        <> contramap (schoolId :: Class -> SchoolId) evText
        <> contramap (createdBy :: Class -> SubjectId) evText


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

insertWordDefinition :: Query (Text, WordDefinition) ()
insertWordDefinition = Q.statement sql encoder D.unit True
  where
    sql = "INSERT INTO dict (word, index, definition, uses_words) \
          \ VALUES ($1, \
              \ (SELECT coalesce(max(index)+1, 0) FROM dict WHERE word = $1), \
              \ $2, \
              \ array(SELECT word::dict_entry FROM unnest ($3, $4) AS word))"
    encoder = contramap fst evText
        <> contramap (fst . snd) evText
        <> contramap (map fst . snd . snd) (eArray E.text)
        <> contramap (map (fromIntegral . snd) . snd . snd) (eArray E.int2)

-- Answers

selectAnswersSql :: ByteString
selectAnswersSql = "SELECT id, story_id, student_id, connect, question, summarise, clarify FROM story_answer"

selectAnswersBySchool :: Query SchoolId [Answer]
selectAnswersBySchool = Q.statement sql evText (D.rowsList answerRow) True
  where
    sql = selectAnswersSql <> " WHERE school_id = $1 :: uuid"

selectAnswersByStudent :: Query (SubjectId, SchoolId) [Answer]
selectAnswersByStudent = Q.statement sql eTextPair (D.rowsList answerRow) True
  where
    sql = selectAnswersSql <> " WHERE student_id = $1 :: uuid AND school_id = $2 :: uuid"

selectAnswersByStory :: Query (SchoolId, StoryId) [Answer]
selectAnswersByStory = Q.statement sql eTextPair (D.rowsList answerRow) True
  where
    sql = selectAnswersSql <> " WHERE school_id = $1 :: uuid AND story_id = $2"

answerRow :: D.Row Answer
answerRow = Answer
    <$> dvUUID
    <*> dvText
    <*> dvUUID
    <*> dvText
    <*> dvText
    <*> dvText
    <*> dvText

insertAnswer :: Query (Answer, SchoolId) ()
insertAnswer = Q.statement sql encode D.unit True
  where
    sql = "INSERT INTO story_answer (id, story_id, student_id, school_id, connect, question, summarise, clarify) \
          \ VALUES ($1 :: uuid, $2, $3 :: uuid, $4 :: uuid, $5, $6, $7, $8)"
    encode = contramap ((id :: Answer -> Text) . fst) evText
        <> contramap ((storyId :: Answer -> Text) . fst) evText
        <> contramap ((studentId :: Answer -> Text) . fst) evText
        <> contramap snd evText
        <> contramap (connect . fst) evText
        <> contramap (question . fst) evText
        <> contramap (summarise . fst) evText
        <> contramap (clarify . fst) evText
