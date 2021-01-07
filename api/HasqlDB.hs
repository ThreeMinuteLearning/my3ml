{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
module HasqlDB where

import           Control.Applicative (liftA2)
import           Control.Exception.Safe
import           Control.Monad (when, replicateM, forM)
import           Control.Monad.Except (catchError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Contravariant.Extras.Contrazip
import           Crypto.Random (getRandomBytes)
import qualified Data.Aeson as JSON
import           Data.Array.IO (IOArray, readArray, writeArray, newListArray)
import           Data.ByteArray.Encoding (convertToBase, convertFromBase, Base(Base16))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Functor.Contravariant
import           Data.Function (on)
import           Data.Int (Int64)
import           Data.List (foldl', groupBy, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import qualified Data.UUID as UUID
import           GHC.Stack (prettySrcLoc)
import           GHC.Stack.Types (HasCallStack, CallStack, getCallStack)
import           Hasql.Statement (Statement)
import           Hasql.Pool (Pool, UsageError(..), use, acquire)
import qualified Hasql.Statement as Q
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import qualified Hasql.Session as S
import           Prelude hiding (id, words)
import           System.Random (randomRIO)
import           Text.Read (readMaybe)

import Api.Types
import DB


mkDB :: String -> IO HasqlDB
mkDB connStr = H <$> acquire (10, 120, BC.pack connStr)

newtype HasqlDB = H Pool

instance DB HasqlDB where
    registerNewAccount Registration {..} userKeys schoolKey db = runSession db $
        case code of
            Nothing -> do
                begin
                schoolId_ <- S.statement (schoolName, TE.decodeUtf8 <$> schoolKey, Nothing) insertSchool

                subId <- S.statement (email, password, schoolAdmin, False) insertAccount
                S.statement (subId, userKeys) insertUserKeys
                S.statement (subId, teacherName, schoolId_) insertTeacher
                commit
                return (Just ())
            Just cd -> do
                begin
                schoolId_ <- S.statement (T.concat (T.split ('-' ==) cd)) getRegistrationCode

                case schoolId_ of
                    Nothing -> S.sql "rollback" >> return Nothing
                    Just sid -> do
                        subId <- S.statement (email, password, teacher, False) insertAccount
                        S.statement (subId, userKeys) insertUserKeys
                        S.statement (subId, teacherName, sid) insertTeacher
                        commit
                        return (Just ())

    createRegistrationCode schoolId_ db = do
        code <- liftIO generateCode
        runStatement insertRegistrationCode (code, schoolId_) db
        return $ T.intercalate "-" (T.chunksOf 4 code)

    getUserKeys = runStatement selectUserKeys

    activateAccount t@(_, accountId) encryptedSchoolKey db = runSession db $ do
        begin
        S.statement t activateTeacherAccount
        S.statement (accountId, TE.decodeUtf8 encryptedSchoolKey) updateSchoolKey
        commit

    loginSuccess subjectId_ keyUpdate db = runSession db $ do
        begin
        S.statement subjectId_ updateLastLogin
        case keyUpdate of
            Nothing -> return ()
            Just sk -> S.statement (subjectId_, TE.decodeUtf8 sk) updateSchoolKey
        commit

    getAccountByUsername = runStatement selectAccountByUsername

    getStories includeDisabled db = do
        stories <- runStatement (selectAllStories includeDisabled) () db
        graph <- runStatement selectStoryGraph () db
        return (StoryData stories graph)

    getStory = runStatement selectStoryById

    createStory story db = runSession db $
        S.statement story insertStory

    updateStory story db = runSession db $ do
        S.statement story updateStory_
        ms <- S.statement ((id :: Story -> StoryId) story) selectStoryById
        updatedFromMaybe ms

    getSchools = runStatement selectAllSchools ()

    getSchool = runStatement selectSchoolById

    getAnthologies = runStatement selectAnthologiesBySchoolId

    createAnthology = runStatement insertAnthology

    updateAnthology = runStatement updateAnthology_

    deleteAnthology anthologyId_ schoolId_ db = do
        nRows <- runStatement deleteAnthologyById (anthologyId_, schoolId_) db
        when (nRows == 0) $ liftIO $ throwDBException "Anthology not found to delete"

    getAnthologyStories = runStatement selectAnthologyStories

    getStarterStories db = do
        anthologyId_ <- runStatement (Q.Statement "SELECT starter_stories FROM config" E.noParams (D.singleRow (D.column (D.nullable D.uuid))) False) () db
        case anthologyId_ of
            Just id_ -> getAnthologyStories (UUID.toText id_) db
            Nothing -> do
                StoryData allStories _ <- getStories False db
                take 25 <$> liftIO (shuffle (take 100 (reverse (sortOn (id :: Story -> StoryId) allStories))))

    setStarterStories = runStatement updateStarterStories

    getClasses = runStatement selectClassesBySchool

    addClassMembers schoolId_ classId_ studentIds db = runSession db $ do
        S.statement (schoolId_, classId_, studentIds) insertClassMembers
        mc <- S.statement classId_ selectClassById
        updatedFromMaybe mc

    removeClassMembers schoolId_ classId_ studentIds db = runSession db $ do
        S.statement (schoolId_, classId_, studentIds) deleteClassMembers
        mc <- S.statement classId_ selectClassById
        updatedFromMaybe mc

    getClass = runStatement selectClassById

    createClass class_ db = do
        result <- runSessionEither db (S.statement class_ insertClass)
        case result of
            Right () -> return (Just ())
            Left (SessionError (S.QueryError _ _ (S.ResultError (S.ServerError "23505" _ _ _)))) -> return Nothing
            Left e -> liftIO $ throwDBException (show e)

    deleteClass classId_ schoolId_ db = do
        nRows <- runStatement deleteClassById (classId_, schoolId_) db
        when (nRows == 0) $ liftIO $ throwDBException "Class not found to delete"

    getStudents = runStatement selectStudentsBySchool

    getStudent schoolId_ studentId_ = runStatement selectStudentById (studentId_, schoolId_)

    getStudentBySubjectId = runStatement selectStudentBySubjectId

    createStudent (nm, lvl, schoolId_) (username, password) db = runSession db $ do
        begin
        subId <- SubjectId . UUID.toText <$> S.statement (username, password, student, True) insertAccount
        createdAt <- liftIO getPOSIXTime
        let s = Student subId nm Nothing lvl schoolId_ False Nothing createdAt
        S.statement (s, subId) insertStudent
        commit
        S.statement subId selectStudentBySubjectId

    updateStudent student_ schoolId_ db = runSession db $ do
        S.statement (student_, schoolId_) updateStudent_
        S.statement ((id :: Student -> SubjectId) student_) selectStudentBySubjectId


    deleteStudent subjectId_ schoolId_ db = runSession db $ do
        begin
        S.statement (subjectId_, schoolId_) $
            Q.Statement "UPDATE student SET deleted=now() WHERE id=$1 :: uuid AND school_id = $2 :: uuid AND deleted is null"
              (contramap fst evSubjectId <> contramap snd evText) D.noResult True
        S.statement subjectId_ $
            Q.Statement "UPDATE login SET locked=true WHERE id=$1 :: uuid"
              evSubjectId D.noResult True
        commit
        S.statement subjectId_ selectStudentBySubjectId

    undeleteStudent subjectId_ schoolId_ db = runSession db $ do
        begin
        S.statement (subjectId_, schoolId_) $
            Q.Statement "UPDATE student SET deleted=null WHERE id=$1 :: uuid AND school_id = $2 :: uuid"
              (contramap fst evSubjectId <> contramap snd evText) D.noResult True
        S.statement subjectId_ $
            Q.Statement "UPDATE login SET locked=false WHERE id=$1 :: uuid"
              evSubjectId D.noResult True
        commit

    setStudentPassword schoolId_ studentId_ password_ =
        runStatement updateStudentPassword (schoolId_, studentId_, password_)

    setStudentUsername schoolId_ studentId_ username_ =
        runStatement updateStudentUsername (schoolId_, studentId_, username_)

    getTeacherBySubjectId = runStatement selectTeacherBySubjectId

    getTeachers = runStatement selectTeachersBySchool

    getDictionary db = do
        elts <- groupBy ((==) `on` fst) <$> runStatement selectDictionary () db
        return $ Map.fromList (map dictWord elts)

    lookupWord = runStatement selectWord

    getAnswers schoolId_ studentId_ storyId_ db = case (studentId_, storyId_) of
        (Just sid, _) -> runStatement selectAnswersByStudent (sid, schoolId_) db
        (_, Just sid) -> runStatement selectAnswersByStory (schoolId_, sid) db
        _ -> runStatement selectAnswersBySchool schoolId_ db

    createAnswer = runStatement insertAnswer

    generateWords db =
      let
        query = Q.Statement "SELECT word FROM word_list WHERE length(word) <= 7 ORDER BY random() LIMIT 4" E.noParams (D.rowList (D.column (D.nonNullable D.text))) True
      in
        runSession db (S.statement () query)

    generateUsername db = do
        let q1 = Q.Statement "SELECT name FROM famous_name ORDER BY random() LIMIT 1" E.noParams (D.singleRow (D.column (D.nonNullable D.text))) True
            q2 = Q.Statement "SELECT replace(username, $1, '') FROM login WHERE username LIKE $1 || '%'" evText (D.rowList (D.column (D.nonNullable D.text))) True
        prefix <- runSession db (S.statement () q1)
        suffixes <- map (fromMaybe (0 :: Int) . readMaybe . T.unpack) <$> runSession db (S.statement prefix q2)
        let newName = case suffixes of
              [] -> prefix
              _ -> T.pack $ T.unpack prefix ++ show (maximum suffixes + 1)
        return newName

    getLeaderBoard = runStatement selectLeaderboardBySchoolId

    updateAccountSettings = runStatement updateAccountSettings_

    getDashboard = runStatement selectDashboard ()

shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArr xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
  where
    n = length xs
    newArr :: [a] -> IO (IOArray Int a)
    newArr = newListArray (1,n)


generateCode :: IO Text
generateCode = do
    codeBytes <- getRandomBytes 8 :: IO ByteString
    let hexCode = convertToBase Base16 codeBytes :: ByteString
    return $ TE.decodeUtf8 hexCode

updatedFromMaybe :: MonadIO m => Maybe a -> m a
updatedFromMaybe (Just a) = return a
updatedFromMaybe Nothing = liftIO $ throwDBException "Couldn't find entity after updating. Shouldn't happen."



dictWord :: [(Text, b)] -> (Text, [b])
dictWord ((w, meaning):ws) = (w, meaning : map snd ws)
dictWord [] = ("", []) -- shouldn't happen


runStatement :: (MonadIO m, HasCallStack) => Statement p a -> p -> HasqlDB -> m a
runStatement q p db = runSession db (S.statement p q)


data DBException = DBException String CallStack

instance Show DBException where
    show (DBException s cs) = concat
        $ "DBException: " : s : ", Called from: " : map go (getCallStack cs)
      where
        go (x, y) = concat [ " ", x, " (", prettySrcLoc y, ")\n" ]

instance Exception DBException

throwDBException :: (MonadThrow m, HasCallStack) => String -> m a
throwDBException msg = throwM (DBException msg ?callStack)

runSession :: (MonadIO m, HasCallStack) => HasqlDB -> S.Session a -> m a
runSession (H pool) s = liftIO $ do
    result <- use pool (rollbackOnError s)
    case result of
        Left e -> liftIO $ throwDBException (show e)
        Right r -> return r

runSessionEither :: (MonadIO m, HasCallStack) => HasqlDB -> S.Session a -> m (Either UsageError a)
runSessionEither (H pool) s = liftIO $ use pool (rollbackOnError s)

rollbackOnError :: S.Session a -> S.Session a
rollbackOnError s_ = s_ `catchError` (\e -> S.sql "rollback;" >> throwError e)

begin :: S.Session ()
begin = S.sql "begin"

commit :: S.Session ()
commit = S.sql "commit"

dvText :: D.Row Text
dvText = D.column (D.nonNullable D.text)

dvNullableText :: D.Row (Maybe Text)
dvNullableText = D.column (D.nullable D.text)

evText :: E.Params Text
evText = E.param (E.nonNullable E.text)

evSubjectId :: E.Params SubjectId
evSubjectId = contramap unSubjectId evText

eTextPair :: E.Params (Text, Text)
eTextPair = contramap fst evText <> contramap snd evText

evUserType :: E.Params UserType
evUserType = contramap userType evText

dvUUID :: D.Row Text
dvUUID = UUID.toText <$> D.column (D.nonNullable D.uuid)

dvSubjectId :: D.Row SubjectId
dvSubjectId = fmap SubjectId dvUUID

evInt4 :: E.Params Int
evInt4 = contramap fromIntegral (E.param (E.nonNullable E.int4))

dvInt4 :: D.Row Int
dvInt4 = fromIntegral <$> D.column (D.nonNullable D.int4)

dvBool :: D.Row Bool
dvBool = D.column (D.nonNullable D.bool)

dArray :: D.Value a -> D.Row [a]
dArray = D.column . D.nonNullable . D.array . D.dimension replicateM . D.element . D.nonNullable

eArray :: Foldable t => E.Value b -> E.Params (t b)
eArray = E.param . E.nonNullable . E.array . E.dimension foldl' . E.element . E.nonNullable


dictEntryValue :: D.Value DictEntry
dictEntryValue = D.composite (DictEntry <$> D.field (D.nonNullable D.text) <*> (fromIntegral <$> D.field (D.nonNullable D.int2)))

dictEntryTupleValue :: D.Value (Text, Int)
dictEntryTupleValue = D.composite ((,) <$> D.field (D.nonNullable D.text) <*> (fromIntegral <$> D.field (D.nonNullable D.int2)))


userTypeValue :: D.Row UserType
userTypeValue = -- D.composite (uType <$> D.compositeValue D.text)
    uType <$> dvText
  where
    uType t = case T.unpack t of
        "Teacher" -> teacher
        "SchoolAdmin" -> schoolAdmin
        "Admin" -> admin
        "Editor" -> editor
        _ -> student

-- User accounts

insertRegistrationCode :: Statement (Text, SchoolId) ()
insertRegistrationCode = Q.Statement sql eTextPair D.noResult False
  where
    sql = "INSERT INTO registration_code (code, school_id) VALUES ($1, $2 :: uuid)"

getRegistrationCode :: Statement Text (Maybe UUID.UUID)
getRegistrationCode = Q.Statement sql evText (D.rowMaybe (D.column (D.nonNullable D.uuid))) False
  where
    sql = "DELETE FROM registration_code \
          \ WHERE code = $1 \
          \ AND created_at > now() - interval '20 minutes'\
          \ RETURNING school_id"


activateTeacherAccount :: Statement (SchoolId, SubjectId) ()
activateTeacherAccount = Q.Statement sql encode D.noResult False
  where
    encode = contramap fst evText
        <> contramap snd evSubjectId
    sql = "UPDATE login SET active = true \
          \ WHERE id = (SELECT id FROM teacher \
                     \  WHERE id = $2 :: uuid \
                     \  AND school_id = $1 :: uuid \
                     \ )"

selectAccountByUsername :: Statement Text (Maybe Account)
selectAccountByUsername = Q.Statement sql evText (D.rowMaybe decode) True
  where
    sql = "SELECT login.id, username, password, otp_key, user_type :: text, level, active, last_login, settings \
          \ FROM login \
          \ LEFT JOIN student \
          \ ON login.id = student.id \
          \ WHERE username = lower($1) AND locked = false"

    decodeOtpKey k = case convertFromBase Base16 (TE.encodeUtf8 k) of
        Left _ -> error "Invalid OTP key in database"
        Right key -> key

    decode = Account
        <$> dvSubjectId
        <*> dvText
        <*> dvText
        <*> (fmap decodeOtpKey <$> dvNullableText)
        <*> userTypeValue
        <*> (maybe 10 fromIntegral <$> D.column (D.nullable D.int2))
        <*> dvBool
        <*> (fmap utcTimeToPOSIXSeconds <$> D.column (D.nullable D.timestamptz))
        <*> D.column (D.nullable D.jsonb)

insertAccount :: Statement (Text, Text, UserType, Bool) UUID.UUID
insertAccount = Q.Statement sql encode (D.singleRow (D.column (D.nonNullable D.uuid))) True
  where
    sql = "INSERT INTO login (username, password, user_type, active) VALUES (lower($1), $2, $3 :: user_type, $4) RETURNING id"
    encode = contrazip4 evText evText evUserType (E.param (E.nonNullable E.bool))

updateLastLogin :: Statement SubjectId ()
updateLastLogin = Q.Statement "UPDATE login SET last_login=now() WHERE id=$1 :: uuid" evSubjectId D.noResult True

updateStudentPassword :: Statement (SchoolId, SubjectId, Text) ()
updateStudentPassword = Q.Statement sql encode D.noResult True
  where
    sql = "UPDATE login SET password = $3 \
          \ WHERE id = (SELECT id from student WHERE id = $2 :: uuid and school_id = $1 :: uuid)"
    encode = contramap (\(sid, _, _) -> sid) evText
        <> contramap (\(_, sid, _) -> sid) evSubjectId
        <> contramap (\(_, _, pwd) -> pwd) evText

updateStudentUsername :: Statement (SchoolId, SubjectId, Text) ()
updateStudentUsername = Q.Statement sql encode D.noResult True
  where
    sql = "UPDATE login SET username = lower($3) \
          \ WHERE id = (SELECT id from student WHERE id = $2 :: uuid and school_id = $1 :: uuid)"
    encode = contramap (\(sid, _, _) -> sid) evText
        <> contramap (\(_, sid, _) -> sid) evSubjectId
        <> contramap (\(_, _, nm) -> nm) evText


selectUserKeys :: Statement SubjectId (Maybe UserKeys)
selectUserKeys = Q.Statement sql evSubjectId (D.rowMaybe userKeysRow) True
  where
    sql = "SELECT salt, pub_key, priv_key, school_key FROM user_keys WHERE user_id=$1 :: uuid"
    decodePublicKey v = case JSON.fromJSON v of
        JSON.Success jwk -> jwk

    userKeysRow = UserKeys
        <$> D.column (D.nonNullable D.bytea)
        <*> (decodePublicKey <$> D.column (D.nonNullable D.jsonb))
        <*> dvText
        <*> (fmap TE.encodeUtf8 <$> dvNullableText)

insertUserKeys :: Statement (UUID.UUID, UserKeys) ()
insertUserKeys = Q.Statement sql encode D.noResult False
  where
    sql = "INSERT INTO user_keys (user_id, salt, pub_key, priv_key, school_key) \
          \ VALUES ($1, $2, $3, $4, $5)"
    encode = contramap fst (E.param (E.nonNullable E.uuid))
        <> contramap snd userKeysEncoder

userKeysEncoder :: E.Params UserKeys
userKeysEncoder = contramap salt (E.param (E.nonNullable E.bytea))
    <> contramap (JSON.toJSON . pubKey) (E.param (E.nonNullable E.jsonb))
    <> contramap privKey evText
    <> contramap (fmap TE.decodeUtf8 . schoolKey) (E.param (E.nullable E.text))

updateSchoolKey :: Statement (SubjectId, Text) ()
updateSchoolKey = Q.Statement sql encode D.noResult False
  where
    sql = "UPDATE user_keys SET school_key=$2 WHERE user_id=$1 :: uuid"
    encode = contramap fst evSubjectId
        <> contramap snd evText

-- Stories

selectStoryGraph :: Statement () [GraphEdge]
selectStoryGraph =
    Q.Statement "SELECT from_story, to_story, description FROM story_graph" mempty (D.rowList decode) True
  where
    decode =
      GraphEdge <$> dvInt4 <*> dvInt4 <*> dvText


selectStorySql :: ByteString
selectStorySql = "SELECT id, title, img_url, level, qualification, curriculum, tags, content, words, clarify_word, enabled, created_at FROM story WHERE not archived"

selectAllStories :: Bool -> Statement () [Story]
selectAllStories includeDisabled =
    Q.Statement sql mempty (D.rowList storyRow) True
  where
    sql
      | includeDisabled = selectStorySql
      | otherwise = selectStorySql <> " AND enabled"

selectStoryById :: Statement StoryId (Maybe Story)
selectStoryById =
    Q.Statement (selectStorySql <> " AND id = $1") evInt4 (D.rowMaybe storyRow) True

storyRow :: D.Row Story
storyRow = Story
    <$> dvInt4
    <*> dvText
    <*> dvText
    <*> (fromIntegral <$> D.column (D.nonNullable D.int2))
    <*> dvNullableText
    <*> dvNullableText
    <*> dArray D.text
    <*> dvText
    <*> dArray dictEntryValue
    <*> dvText
    <*> dvBool
    <*> (round . utcTimeToPOSIXSeconds <$> D.column (D.nonNullable D.timestamptz))

insertStory :: Statement Story StoryId
insertStory = Q.Statement sql storyEncoder (D.singleRow dvInt4) True
  where
    sql = "INSERT INTO story (title, img_url, level, qualification, curriculum, tags, content, words, clarify_word) \
                 \VALUES ($2, $3, $4, $5, $6, $7, $8, (array(select word::dict_entry from unnest ($9, $10) as word)), $11) \
                 \RETURNING id"

updateStory_ :: Statement Story ()
updateStory_ = Q.Statement sql storyEncoder D.noResult True
  where
    sql = "UPDATE story SET title=$2, img_url=$3, level=$4, qualification=$5, curriculum=$6, tags=$7, content=$8, words=(array(select word::dict_entry from unnest ($9, $10) as word)), clarify_word=$11, enabled=$12 WHERE id=$1"

storyEncoder :: E.Params Story
storyEncoder = contramap (id :: Story -> StoryId) evInt4
    <> contramap title evText
    <> contramap img evText
    <> contramap storyLevel evInt4
    <> contramap qualification (E.param (E.nullable E.text))
    <> contramap curriculum (E.param (E.nullable E.text))
    <> contramap tags (eArray E.text)
    <> contramap content evText
    <> contramap (map word . words) (eArray E.text)
    <> contramap (map (fromIntegral . index) . words) (eArray E.int2)
    <> contramap clarifyWord evText
    <> contramap enabled (E.param (E.nonNullable E.bool))
    -- <> contramap date (E.param E.timestamptz)
  where
    storyLevel = level :: Story -> Int

-- School

insertSchool :: Statement (Text, Maybe Text, Maybe Text) UUID.UUID
insertSchool = Q.Statement sql encode (D.singleRow (D.column (D.nonNullable D.uuid))) False
  where
    sql = "INSERT INTO school (name, school_key, description) values ($1, $2, $3) RETURNING id"
    encode = contrazip3 evText
        (E.param (E.nullable E.text))
        (E.param (E.nullable E.text))

-- Teachers

selectTeacherSql :: ByteString
selectTeacherSql = "SELECT id, name, bio, school_id FROM teacher"

selectTeacherBySubjectId :: Statement SubjectId Teacher
selectTeacherBySubjectId = Q.Statement sql evSubjectId (D.singleRow teacherRow) True
  where
    sql = selectTeacherSql <> " WHERE id = $1 :: uuid"

selectTeachersBySchool :: Statement SchoolId [(Teacher, Bool)]
selectTeachersBySchool = Q.Statement sql evText (D.rowList (liftA2 (,) teacherRow dvBool)) True
  where
    sql = "SELECT t.id, t.name, t.bio, t.school_id, a.active \
          \ FROM teacher as t \
          \ JOIN login as a \
          \ ON t.id = a.id \
          \ WHERE school_id = $1 :: uuid "


teacherRow :: D.Row Teacher
teacherRow = Teacher
    <$> dvSubjectId
    <*> dvText
    <*> dvNullableText
    <*> dvUUID

insertTeacher :: Statement (UUID.UUID, Text, UUID.UUID) ()
insertTeacher = Q.Statement sql encode D.noResult False
  where
    sql = "INSERT INTO teacher (id, name, school_id) VALUES ($1, $2, $3)"
    encode = contrazip3 (E.param (E.nonNullable E.uuid)) evText (E.param (E.nonNullable E.uuid))

-- Students

selectStudentSql :: ByteString
selectStudentSql = "SELECT id, name, description, level, school_id, hidden, deleted, created_at FROM student"

selectStudentsBySchool :: Statement SchoolId [Student]
selectStudentsBySchool = Q.Statement sql evText (D.rowList studentRow) True
  where
    sql = selectStudentSql <> " WHERE school_id = $1 :: uuid"

selectStudentById :: Statement (SubjectId, SchoolId) (Maybe Student)
selectStudentById = Q.Statement sql encode (D.rowMaybe studentRow) True
  where
    encode = contramap fst evSubjectId
        <> contramap snd evText
    sql = selectStudentSql <> " WHERE id = $1 :: uuid AND school_id = $2 :: uuid"

selectStudentBySubjectId :: Statement SubjectId Student
selectStudentBySubjectId = Q.Statement sql evSubjectId (D.singleRow studentRow) True
  where
    sql = selectStudentSql <> " WHERE id = $1 :: uuid"

studentRow :: D.Row Student
studentRow = Student
    <$> dvSubjectId
    <*> dvText
    <*> dvNullableText
    <*> (fromIntegral <$> D.column (D.nonNullable D.int2))
    <*> dvUUID
    <*> dvBool
    <*> (fmap utcTimeToPOSIXSeconds <$> D.column (D.nullable D.timestamptz))
    <*> (utcTimeToPOSIXSeconds <$> D.column (D.nonNullable D.timestamptz))

insertStudent :: Statement (Student, SubjectId) ()
insertStudent = Q.Statement sql encode D.noResult True
  where
    sql = "INSERT INTO student (id, name, description, level, school_id) VALUES ($1 :: uuid, $2, $3, $4, $5 :: uuid)"
    encode = contramap snd evSubjectId
        <> contramap ((name :: Student -> Text) . fst) evText
        <> contramap ((description :: Student -> Maybe Text) . fst) (E.param (E.nullable E.text))
        <> contramap ((fromIntegral . (level :: Student -> Int)) . fst) (E.param (E.nonNullable E.int4))
        <> contramap ((schoolId :: Student -> SchoolId) . fst) evText

updateStudent_ :: Statement (Student, SchoolId) ()
updateStudent_ = Q.Statement sql encode D.noResult True
  where
    sql = "UPDATE student SET level=$3, hidden=$4 \
          \ WHERE id=$5 :: uuid AND school_id=$6 :: uuid"
    encode = contramap ((name :: Student -> Text) . fst) evText
        <> contramap ((description :: Student -> Maybe Text) . fst) (E.param (E.nullable E.text))
        <> contramap ((fromIntegral . (level :: Student -> Int)) . fst) (E.param (E.nonNullable E.int4))
        <> contramap ((hidden :: Student -> Bool) . fst) (E.param (E.nonNullable E.bool))
        <> contramap ((id :: Student -> SubjectId) . fst) evSubjectId
        <> contramap snd evText

-- Schools

selectSchoolSql :: ByteString
selectSchoolSql = "SELECT id, name, description FROM school"

selectAllSchools :: Statement () [School]
selectAllSchools =
    Q.Statement selectSchoolSql mempty (D.rowList schoolRow) True

selectSchoolById :: Statement SchoolId (Maybe School)
selectSchoolById =
    Q.Statement (selectSchoolSql <> " WHERE id = $1 :: uuid") evText (D.rowMaybe schoolRow) True

schoolRow :: D.Row School
schoolRow = School
    <$> dvUUID
    <*> dvText
    <*> dvNullableText


-- Classes

selectClassSql :: ByteString
selectClassSql = "SELECT id, name, description, school_id, created_by, array(SELECT student_id :: text FROM student_class WHERE class_id = class.id) AS students FROM class"

selectClassesBySchool :: Statement SchoolId [Class]
selectClassesBySchool = Q.Statement sql evText (D.rowList classRow) True
  where
    sql = selectClassSql <> " WHERE school_id = $1 :: uuid"

selectClassById :: Statement ClassId (Maybe Class)
selectClassById = Q.Statement sql evText (D.rowMaybe classRow) True
  where
    sql = selectClassSql <> " WHERE id = $1 :: uuid"

classRow :: D.Row Class
classRow = Class
    <$> dvUUID
    <*> dvText
    <*> dvNullableText
    <*> dvUUID
    <*> dvSubjectId
    <*> dArray (SubjectId <$> D.text)

insertClassMembers :: Statement (SchoolId, ClassId, [SubjectId]) ()
insertClassMembers = Q.Statement sql encode D.noResult True
  where
    sql = "INSERT INTO student_class (school_id, class_id, student_id) \
          \ SELECT $1 :: uuid, $2 :: uuid, studentId FROM unnest ($3 :: uuid[]) as studentId \
          \ ON CONFLICT DO NOTHING"
    encode = contramap (\(sid, _, _) -> sid) evText
        <> contramap (\(_, cid, _) -> cid) evText
        <> contramap (\(_, _, sids) -> map unSubjectId sids) (eArray E.text)

deleteClassMembers :: Statement (SchoolId, ClassId, [SubjectId]) ()
deleteClassMembers = Q.Statement sql encode D.noResult True
  where
    sql = "DELETE FROM student_class \
          \ WHERE class_id=$2 :: uuid \
          \ AND school_id=$1 :: uuid \
          \ AND student_id IN (SELECT studentId FROM unnest ($3 :: uuid[]) as studentId)"
    encode = contramap (\(sid, _, _) -> sid) evText
        <> contramap (\(_, cid, _) -> cid) evText
        <> contramap (\(_, _, sids) -> map unSubjectId sids) (eArray E.text)



insertClass :: Statement Class ()
insertClass = Q.Statement sql encode D.noResult True
  where
    sql = "INSERT INTO class (id, name, description, school_id, created_by) values ($1 :: uuid, $2, $3, $4 :: uuid, $5 :: uuid)"
    encode = contramap (id :: Class -> ClassId) evText
        <> contramap (name :: Class -> Text) evText
        <> contramap (description :: Class -> Maybe Text) (E.param (E.nullable E.text))
        <> contramap (schoolId :: Class -> SchoolId) evText
        <> contramap (createdBy :: Class -> SubjectId) evSubjectId

deleteClassById :: Statement (ClassId, SchoolId) Int64
deleteClassById = Q.Statement sql eTextPair D.rowsAffected True
  where
    sql = "DELETE FROM class WHERE id = $1 :: uuid AND school_id = $2 :: uuid"



-- Anthologies

selectAnthologiesBySchoolId :: Statement (Maybe SchoolId) [Anthology]
selectAnthologiesBySchoolId = Q.Statement sql (E.param (E.nullable E.text)) (D.rowList anthologyRow) True
  where
    sql = "SELECT id, name, description, created_by, school_id :: text, stories, hidden \
          \ FROM anthology \
          \ WHERE school_id is null \
          \ OR school_id = $1 :: uuid \
          \ ORDER BY school_id, created_at DESC"

anthologyRow :: D.Row Anthology
anthologyRow = Anthology
    <$> dvUUID
    <*> dvText
    <*> dvText
    <*> (SubjectId <$> dvUUID)
    <*> dvNullableText
    <*> (map fromIntegral <$> dArray D.int4)
    <*> dvBool

anthologyEncoder :: E.Params Anthology
anthologyEncoder = contramap (id :: Anthology -> AnthologyId) evText
    <> contramap (name :: Anthology -> Text) evText
    <> contramap (description :: Anthology -> Text) evText
    <> contramap (unSubjectId . (createdBy :: Anthology -> SubjectId)) evText
    <> contramap (schoolId :: Anthology -> Maybe SchoolId) (E.param (E.nullable E.text))
    <> contramap (map fromIntegral <$> (stories :: Anthology -> [StoryId])) (eArray E.int4)
    <> contramap (hidden :: Anthology -> Bool) (E.param (E.nonNullable E.bool))

insertAnthology :: Statement Anthology ()
insertAnthology = Q.Statement sql anthologyEncoder D.noResult True
  where
    sql = "INSERT INTO anthology (id, name, description, created_by, school_id, stories, hidden) \
              \ VALUES ($1 :: uuid, $2, $3, $4 :: uuid, $5 :: uuid, $6, $7)"

updateAnthology_ :: Statement Anthology ()
updateAnthology_ = Q.Statement sql anthologyEncoder D.noResult True
  where
    sql = "UPDATE anthology SET name=$2, stories=$6, hidden=$7 WHERE id=$1 :: uuid"

deleteAnthologyById :: Statement (AnthologyId, Maybe SchoolId) Int64
deleteAnthologyById = Q.Statement sql encode D.rowsAffected True
  where
    sql = "DELETE FROM anthology \
          \ WHERE id = $1 :: uuid \
          \ AND ($2 is null or school_id = $2 :: uuid)"
    encode = contramap fst evText
        <> contramap snd (E.param(E.nullable E.text))


selectAnthologyStories :: Statement AnthologyId [Story]
selectAnthologyStories = Q.Statement sql evText (D.rowList storyRow) True
  where
    sql = selectStorySql <> " AND id in\
                            \ ( SELECT unnest(stories) FROM anthology \
                            \   WHERE id = $1 :: uuid\
                            \ )"


updateStarterStories :: Statement AnthologyId ()
updateStarterStories =
    Q.Statement "UPDATE config SET starter_stories = $1 :: uuid" evText D.noResult False

-- Word dictionary

selectDictionary :: Statement () [(Text, WordDefinition)]
selectDictionary = Q.Statement sql mempty decode True
  where
    sql = "SELECT word, index, definition, uses_words FROM dict ORDER BY word, index"
    decode = D.rowList $ do
        word_ <- dvText
        _ <- D.column (D.nonNullable D.int2)
        defn <- dvText
        uses <- dArray dictEntryTupleValue
        return (word_, (defn, uses))

selectWord :: Statement Text [WordDefinition]
selectWord = Q.Statement sql evText decode True
  where
    sql = "SELECT index, definition, uses_words \
          \ FROM dict \
          \ WHERE word = $1 \
          \ ORDER BY index"
    decode = D.rowList $ D.column (D.nonNullable D.int2) >> (,) <$> dvText <*> dArray dictEntryTupleValue

insertWordDefinition :: Statement (Text, WordDefinition) ()
insertWordDefinition = Q.Statement sql encoder D.noResult True
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
selectAnswersSql = "SELECT story_id, student_id, connect, question, summarise, clarify, created_at FROM story_answer"

selectAnswersBySchool :: Statement SchoolId [Answer]
selectAnswersBySchool = Q.Statement sql evText (D.rowList answerRow) True
  where
    sql = selectAnswersSql <> " WHERE school_id = $1 :: uuid"

selectAnswersByStudent :: Statement (SubjectId, SchoolId) [Answer]
selectAnswersByStudent = Q.Statement sql encode (D.rowList answerRow) True
  where
    encode = contramap fst evSubjectId
        <> contramap snd evText
    sql = selectAnswersSql <> " WHERE student_id = $1 :: uuid AND school_id = $2 :: uuid ORDER BY created_at DESC"

selectAnswersByStory :: Statement (SchoolId, StoryId) [Answer]
selectAnswersByStory = Q.Statement sql encode (D.rowList answerRow) True
  where
    encode = contramap fst evText
        <> contramap (fromIntegral . snd) (E.param (E.nonNullable E.int4))
    sql = selectAnswersSql <> " WHERE school_id = $1 :: uuid AND story_id = $2 ORDER BY created_at DESC"

answerRow :: D.Row Answer
answerRow = Answer
    <$> dvInt4
    <*> dvSubjectId
    <*> dvText
    <*> dvText
    <*> dvText
    <*> dvText
    <*> (round . utcTimeToPOSIXSeconds <$> D.column (D.nonNullable D.timestamptz))

insertAnswer :: Statement (Answer, SchoolId) ()
insertAnswer = Q.Statement sql encode D.noResult True
  where
    sql = "INSERT INTO story_answer (story_id, student_id, school_id, connect, question, summarise, clarify) \
          \ VALUES ($1, $2 :: uuid, $3 :: uuid, $4, $5, $6, $7)"
    encode = contramap (fromIntegral . (storyId :: Answer -> Int) . fst) (E.param (E.nonNullable E.int4))
        <> contramap ((studentId :: Answer -> SubjectId) . fst) evSubjectId
        <> contramap snd evText
        <> contramap (connect . fst) evText
        <> contramap (question . fst) evText
        <> contramap (summarise . fst) evText
        <> contramap (clarify . fst) evText

-- Leaderboard

selectLeaderboardBySchoolId :: Statement SchoolId [LeaderBoardEntry]
selectLeaderboardBySchoolId = Q.Statement sql evText (D.rowList decode) True
  where
    sql = "SELECT position, name, student_id, score FROM leaderboard WHERE school_id=$1 :: uuid"
    decode = LeaderBoardEntry
        <$> dvInt4
        <*> dvText
        <*> dvSubjectId
        <*> dvInt4

-- Account settings
updateAccountSettings_ :: Statement (SubjectId, JSON.Value) ()
updateAccountSettings_ = Q.Statement sql encode D.noResult True
  where
    sql = "UPDATE login SET settings=$2 where id = $1 :: uuid"
    encode = contramap fst evSubjectId
        <> contramap snd (E.param (E.nonNullable E.jsonb))

-- Admin
selectDashboard :: Statement () JSON.Value
selectDashboard = Q.Statement "SELECT dashboard()" E.noParams (D.singleRow (D.column (D.nonNullable D.json))) True
