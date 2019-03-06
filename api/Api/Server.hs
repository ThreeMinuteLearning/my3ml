{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Server
    ( server
    , Config (..)
    , HandlerT
    ) where
import           Control.Concurrent
import           Control.Error
import           Control.Monad (unless)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.KDF.Argon2 (Options(..), defaultOptions, hash)
import           Crypto.Error
import qualified Crypto.OTP as OTP
import           Crypto.Random (getRandomBytes)
import qualified Data.Aeson as JSON
import qualified Data.ByteArray as BA
import           Data.ByteArray (ScrubbedBytes)
import           Data.ByteArray.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (isNothing)
import           Data.Monoid ((<>))
import           Data.List (scanl', last, uncons)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Time.Clock.POSIX
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Jose.Jwt hiding (encode, decode)
import           Jose.Jwa
import           Jose.Jwk
import qualified Jose.Jwe as Jwe
import           Jose.Internal.Crypto (keyWrap, keyUnwrap)
import           Prelude hiding (id, words)
import           Servant ((:<|>) ((:<|>)), ServerT, ServantErr(..), Handler, NoContent(..), err400, err401, err403, err409, err404, err500, errBody)

import           Api.Auth (AccessScope(..), TenantKey(..), mkAccessToken, scopeSubjectId)
import           Api.Types hiding (AccessToken)
import           DB (DB)
import qualified DB
import           Password
import qualified Rollbar

data Config db = Config
    { database :: db
    , tokenKey :: Jwk
    , sampleStories :: MVar [Story]
    , rollbarSettings :: Maybe Rollbar.Settings
    , rootKey :: Maybe Jwk
    }

type HandlerT db = LoggingT (ReaderT (Config db) Handler)

type ApiServer a db = ServerT a (HandlerT db)

runDB :: MonadReader (Config b) m => (b -> m b1) -> m b1
runDB f = ask >>= f . database

server :: DB db => ApiServer Api db
server = storyServer :<|> dictServer :<|> schoolsServer :<|> schoolServer :<|> anthologiesServer :<|> loginServer :<|> accountServer :<|> adminServer

newUUID :: HandlerT db Text
newUUID = liftIO (toText <$> nextRandom)

deriveKey :: BA.ByteArrayAccess b2 => B.ByteString -> b2 -> ScrubbedBytes
deriveKey salt password = pbeKeyBytes :: ScrubbedBytes
  where
    CryptoPassed pbeKeyBytes = hash kdfOptions password salt 32
    kdfOptions = defaultOptions { iterations = 3, parallelism = 1, memory = 4096}

encryptSchoolKey :: ScrubbedBytes -> ScrubbedBytes -> B.ByteString
encryptSchoolKey pbeKey schoolKey = convertToBase Base64URLUnpadded pbeSchoolKey
  where
    Right pbeSchoolKey = keyWrap A256KW pbeKey schoolKey :: Either JwtError B.ByteString

decryptSchoolKey :: ScrubbedBytes -> B.ByteString -> ScrubbedBytes
decryptSchoolKey pbeKey esk = sk
  where
    Right rawBytes = convertFromBase Base64URLUnpadded esk :: Either String B.ByteString
    Right sk = keyUnwrap pbeKey A256KW rawBytes

encryptRsaKey :: (BA.ByteArrayAccess bin, MonadIO m) => Jwk -> bin -> m B.ByteString
encryptRsaKey kPr pbeKey = do
    encoded <- liftIO $ Jwe.jwkEncode A256KW A256GCM (SymmetricJwk (BA.convert pbeKey) Nothing Nothing Nothing) (Claims (BL.toStrict (JSON.encode kPr)))
    case encoded of
        Right (Jwt eKpr) -> return eKpr
        _ -> error "Failed to encrypt RSA key"

decryptSchoolKeyWithRsaKey :: ScrubbedBytes -> B.ByteString -> B.ByteString -> IO ScrubbedBytes
decryptSchoolKeyWithRsaKey pbeKey rsaKeyJwt schoolKey = do
    Right (Jwe (_, jwk)) <- Jwe.jwkDecode (SymmetricJwk (BA.convert pbeKey) Nothing Nothing Nothing) rsaKeyJwt
    let Right rsaJwk = JSON.eitherDecodeStrict jwk
    Right (Jwe (_, sk)) <- Jwe.jwkDecode rsaJwk schoolKey
    return (BA.convert sk)

encryptSchoolKeyWithRsaKey :: ScrubbedBytes -> Jwk -> IO B.ByteString
encryptSchoolKeyWithRsaKey schoolKey pubKey = do
    Right (Jwt eKpr) <- Jwe.jwkEncode RSA_OAEP A256GCM pubKey (Claims $ BA.convert schoolKey)
    return eKpr

loginServer :: DB db => ApiServer LoginApi db
loginServer authReq = do
    logInfoN $ "Login request from: " <> uName
    user <- runDB $ DB.getAccountByUsername uName
    let submittedPassword = encodeUtf8 (password (authReq :: LoginRequest))
    case user of
        Nothing -> logInfoN ("User not found: " <> uName) >> throwError err401
        Just a@Account {..} -> do
            when (isJust otpKey && isNothing (otp authReq)) (throwError err462)
            otpTime <- liftIO getOTPTime
            let passwordOK = validatePassword submittedPassword (encodeUtf8 password)
                otpOK = validateOTP (fromIntegral <$> otp authReq) otpKey otpTime
            unless passwordOK $ logInfoN ("Wrong password for user " <> uName)
            unless otpOK $ logInfoN ("Wrong otp code for user " <> uName)
            unless (passwordOK && otpOK) $ throwError err401
            -- new account which hasn't been enabled yet
            unless active (logInfoN ("Account " <> uName <> " has not been activated yet") >> throwError err403)
            let firstLogin = isNothing lastLogin
            keys <- getUserKeys id firstLogin role submittedPassword
            (accessToken, nm) <- createToken a (fmap snd keys)
            let keyUpdate = fmap (uncurry encryptSchoolKey) keys
            runDB $ DB.loginSuccess id keyUpdate
            logInfoN ("User successfuly authenticated: " <> uName)
            return $ Login id uName nm role level settings accessToken
  where
    -- getUserKeys :: Monad m => SubjectId -> Bool -> UserType -> B.ByteString -> m (Maybe (ScrubbedBytes, ScrubbedBytes))
    getUserKeys id firstLogin role submittedPassword = runMaybeT $ do
        unless (role == teacher || role == schoolAdmin) nothing
        keys <- runDB $ DB.getUserKeys id
        ks <- hoistMaybe keys
        let pbeKey = deriveKey (salt ks) submittedPassword
        esk <- hoistMaybe (schoolKey ks)
        sk <- if firstLogin && role == teacher
              then lift . liftIO $ decryptSchoolKeyWithRsaKey pbeKey (encodeUtf8 (privKey ks)) esk
              else pure $ decryptSchoolKey pbeKey esk

        just (pbeKey, sk)

    -- 462 is an arbitrarily chosen code to tell our client an OTP is required for the user
    err462 = ServantErr
        { errHTTPCode = 462
        , errReasonPhrase = "OTP Auth Required"
        , errBody = ""
        , errHeaders = []
        }

    getOTPTime = getPOSIXTime >>= \t -> pure (floor t :: OTP.OTPTime)

    validateOTP Nothing Nothing _ = True
    validateOTP (Just otp) (Just key) t = OTP.totpVerify OTP.defaultTOTPParams key t otp
    validateOTP _ _ _ = error "Invalid OTP combination"

    uName = T.toLower . T.strip $ username (authReq :: LoginRequest)

    getTeacher subId isAdmin userKey = case userKey of
        Nothing -> logErrorN "Teacher account with no tenant key" >> throwError err500
        Just sk -> do
            Teacher {..} <- runDB $ DB.getTeacherBySubjectId subId
            return (TeacherScope subId schoolId (TenantKey sk) isAdmin, name)

    createToken acct userKey = do
        let subId = id (acct :: Account)
        (scope, nm) <- case userType (role (acct :: Account)) of
            "Student" -> do
                 stdnt <- runDB $ DB.getStudentBySubjectId subId
                 return (StudentScope subId (schoolId (stdnt :: Student)), name (stdnt :: Student))
            "Teacher" -> getTeacher subId False userKey

            "SchoolAdmin" -> getTeacher subId True userKey
            "Editor" -> return (EditorScope subId, "Anonymous")
            "Admin" -> pure (AdminScope subId, uName)
            x -> error ("Unexpected user type " ++ show x)
        jwk <- fmap tokenKey ask
        token_ <- mkAccessToken jwk scope
        return (token_, nm)

accountServer :: DB db => ApiServer AccountApi db
accountServer token_ =
    case token_ of
        Nothing -> throwAll err401 :<|> registerNewAccount :<|> throwAll err401
        Just t -> updateSettings (scopeSubjectId t) :<|> throwAll err403 :<|> newRegistrationCode t
  where
    updateSettings userId newSettings = do
        runDB $ DB.updateAccountSettings (userId, newSettings)
        return NoContent

    registerNewAccount r = do
        logInfoN $ "New registration for: " <> email r
        existing <- runDB $ DB.getAccountByUsername (email r)
        unless (isNothing existing) $ logInfoN "Account already exists" >> throwError err409
        salt <- liftIO (getRandomBytes 32)
        let submittedPassword = encodeUtf8 (password (r :: Registration))
            pbeKey = deriveKey salt submittedPassword
        hashedPassword <- fmap decodeUtf8 $ liftIO $ hashPassword passwordOptions submittedPassword
        (kPub, kPr) <- liftIO $ generateRsaKeyPair 256 (KeyId "rsa") Enc Nothing
        ekPr <- encryptRsaKey kPr pbeKey
        schoolKeys <- case code r of
            Just _ -> return Nothing
            Nothing -> Just <$> newSchoolKey pbeKey

        let userKeys = UserKeys salt kPub (decodeUtf8 ekPr) (fst <$> schoolKeys)
        result <- runDB $ DB.registerNewAccount (r { password = hashedPassword } ) userKeys (join (fmap snd schoolKeys))
        case result of
            Just _ -> return NoContent
            Nothing -> throwError err403

    newSchoolKey :: ScrubbedBytes -> HandlerT db (B.ByteString, Maybe B.ByteString)
    newSchoolKey pbeKey = do
        bytes <- liftIO (getRandomBytes 32)
        rootKey_ <- fmap rootKey ask
        backupKey <- case rootKey_ of
            Nothing -> return Nothing
            Just k -> fmap Just $ liftIO $ encryptSchoolKeyWithRsaKey bytes k
        return (encryptSchoolKey pbeKey bytes, backupKey)


    newRegistrationCode t = case t of
        TeacherScope _ schoolId_ _ True ->
            runDB $ DB.createRegistrationCode schoolId_
        _ -> throwError err403

    passwordOptions = defaultOptions { iterations = 3, parallelism = 1, memory = 4096}

storyServer :: DB db => ApiServer StoriesApi db
storyServer token_ =
    getStories :<|> specificStoryServer :<|> createStory
  where
    notFound = err404 { errBody = "Story with this ID was not found" }

    specificStoryServer storyId_ = getStory storyId_ :<|> updateStory storyId_

    getStories =
        case token_ of
            Nothing -> do
                cfg <- ask
                liftIO (readMVar (sampleStories cfg))
            Just (EditorScope _) -> runDB (DB.getStories True)
            _ -> runDB (DB.getStories False)

    getStory storyId_ = do
        story <- runDB (DB.getStory storyId_)
        case story of
            Nothing -> throwError notFound
            Just s -> return s

    updateStory storyId_ story@Story{..} = do
        logInfoN $ "Updating story: " <> (T.pack . show) storyId_ <> " " <> title
        case token_ of
            Just (EditorScope _) -> runDB (DB.updateStory (story { id = storyId_} ))
            _ -> throwError err403

    createStory story@Story{..} = do
        logInfoN $ "Creating new story: " <> title
        newId <- runDB (DB.createStory story)
        return (story { id = newId } :: Story)


anthologiesServer :: DB db => ApiServer AnthologiesApi db
anthologiesServer Nothing = throwAll err401
anthologiesServer (Just (TeacherScope subId sid _ _)) =
     getAnthologiesForSchool (Just sid)
     :<|> createAnthology subId (Just sid)
     :<|> (\aid ->
               throwAll err403
          :<|> updateAnthology (Just sid) aid
          :<|> deleteAnthology (Just sid) aid
          )
anthologiesServer (Just (StudentScope _ sid)) =
     getAnthologiesForSchool (Just sid) :<|> throwAll err403 :<|> throwAll err403
anthologiesServer (Just (EditorScope subId)) =
     getGlobalAnthologies
     :<|> createAnthology subId Nothing
     :<|> (\aid ->
               setStarterStories aid
               :<|> updateAnthology Nothing aid
               :<|> deleteAnthology Nothing aid
          )
anthologiesServer _ = throwAll err403


getGlobalAnthologies :: DB db => HandlerT db [Anthology]
getGlobalAnthologies = runDB $ DB.getAnthologies Nothing

getAnthologiesForSchool :: DB db => Maybe SchoolId -> HandlerT db [Anthology]
getAnthologiesForSchool = fmap (filter (not . (hidden :: Anthology -> Bool))) . runDB . DB.getAnthologies

createAnthology :: DB db => SubjectId -> Maybe SchoolId -> Anthology -> HandlerT db Anthology
createAnthology subId sid anthology = do
    uuid <- liftIO (toText <$> nextRandom)
    let anthologyWithId = anthology { id = uuid, createdBy = subId, schoolId = sid } :: Anthology
    _ <- runDB (DB.createAnthology anthologyWithId)
    return anthologyWithId

updateAnthology :: DB db => Maybe SchoolId -> AnthologyId -> Anthology -> HandlerT db Anthology
updateAnthology sid aid anthology = do
    let anthologyWithId = anthology { id = aid, schoolId = sid } :: Anthology
    _ <- runDB (DB.updateAnthology anthologyWithId)
    return anthologyWithId

setStarterStories :: DB db => AnthologyId -> HandlerT db NoContent
setStarterStories aid = do
    logInfoN $ "Setting starter stories to anthology: " <> aid
    stories_ <- runDB (DB.getAnthologyStories aid)
    runDB (DB.setStarterStories aid)
    mvar <- sampleStories <$> ask
    liftIO $ takeMVar mvar >> putMVar mvar stories_
    return NoContent


deleteAnthology :: DB db => Maybe SchoolId -> AnthologyId -> HandlerT db AnthologyId
deleteAnthology sid aid = do
    _ <- runDB (DB.deleteAnthology aid sid)
    return aid


schoolsServer :: DB db => ApiServer SchoolsApi db
schoolsServer Nothing = throwAll err401
schoolsServer (Just scp@(AdminScope _)) = runDB DB.getSchools :<|> specificSchoolServer scp
schoolsServer _ = throwAll err403



schoolServer :: DB db => ApiServer SchoolApi db
schoolServer scope = case scope of
    Nothing -> throwAll err401
    Just scp@(TeacherScope _ sid _ _) -> specificSchoolServer scp sid
    Just scp@(StudentScope _ sid) ->
        throwAll err403
        :<|> throwAll err403
        :<|> answersServer scp
        :<|> leaderBoardServer sid
        :<|> throwAll err403
    _ -> throwAll err403


specificSchoolServer :: DB db => AccessScope -> SchoolId -> ApiServer (ClassesApi :<|> StudentsApi :<|> AnswersApi :<|> LeaderBoardApi :<|> TeachersApi) db
specificSchoolServer scp sid = classesServer (scopeSubjectId scp) sid :<|> studentsServer scp sid :<|> answersServer scp :<|> leaderBoardServer sid :<|> teachersServer scp sid


classesServer :: DB db => SubjectId -> SchoolId -> ApiServer ClassesApi db
classesServer subId sid = runDB (DB.getClasses sid) :<|> specificClassServer :<|> createClass
  where
    specificClassServer cid = getClass cid :<|> deleteClass cid :<|> setClassMembers cid

    getClass cid = do
        c <- runDB (DB.getClass cid)
        maybe (throwError err404) return c

    deleteClass cid = do
         _ <- runDB (DB.deleteClass cid sid)
         return NoContent

    setClassMembers cid studentIds delete =
        if delete == Just True
            then
                runDB (DB.removeClassMembers sid cid studentIds)
            else
                runDB (DB.addClassMembers sid cid studentIds)

    createClass (nm, desc) = do
        uuid <- newUUID
        let c = Class uuid nm (Just desc) sid subId []
        result <- runDB (DB.createClass c)
        case result of
            Nothing -> throwError err409
            Just _ -> return c;


studentsServer :: DB db => AccessScope -> SchoolId -> ApiServer StudentsApi db
studentsServer scp@(TeacherScope _ _ (TenantKey key) _) schoolId_ = getStudents :<|> specificStudentServer :<|> createStudents
  where
    specificStudentServer studId = getStudent studId :<|> updateStudent studId :<|> changePassword studId :<|> changeUsername studId :<|> studentAdmin studId scp

    studentAdmin _ (TeacherScope _ _ _ False) = throwAll err403
    studentAdmin studId _ = deleteStudent studId :<|> undelete studId

    getStudents = map decryptStudent <$> runDB (DB.getStudents schoolId_)

    getStudent studId = do
        s <- runDB $ DB.getStudent schoolId_ studId
        maybe (throwError err404) return (fmap decryptStudent s)

    changePassword studId password_ = do
        logInfoN $ "Setting password for student: " <> unSubjectId studId
        when (T.length password_ < 8) (throwError err400)
        hashedPassword <- encodePassword password_
        runDB $ DB.setStudentPassword schoolId_ studId hashedPassword
        return NoContent

    changeUsername studId username_ = do
        logInfoN $ "Setting username for student: " <> unSubjectId studId <> " to " <> username_
        when (T.length username_ < 3) (throwError err400)

        user <- runDB $ DB.getAccountByUsername username_
        case user of
            Nothing -> runDB $ DB.setStudentUsername schoolId_ studId username_
            Just u ->
                unless (id (u :: Account) == studId) (throwError err409)
        return NoContent

    updateStudent _ s@Student{..} = do
        logInfoN $ "Updating student: " <> unSubjectId id
        eStudent <- encryptStudent s
        dbStudent <- runDB $ DB.updateStudent eStudent schoolId_
        return $ decryptStudent dbStudent

    deleteStudent studId = do
        logInfoN $ "Deleting student: " <> unSubjectId studId
        s <- runDB $ DB.deleteStudent studId schoolId_
        return $ decryptStudent s

    undelete studId = do
        logInfoN $ "Un-deleting student: " <> unSubjectId studId
        runDB $ DB.undeleteStudent studId schoolId_
        return NoContent

    generatePassword minLength = do
        ws <- take 4 <$> runDB DB.generateWords
        let lengths = map T.length ws
            cumlLengths = scanl' (+) (head lengths) (tail lengths)
            joinWithDash w1 w2 = w1 <> "-" <> w2
            cumlWords = scanl' joinWithDash (head ws) (tail ws)
            cumlWordsLengths = zip cumlWords cumlLengths
            pass =
                case uncons (dropWhile ((< minLength) . snd) cumlWordsLengths) of
                    Just ((p, _), _) -> p
                    Nothing -> last cumlWords

        return (T.toLower pass)

    createStudents (level_, names) = mapM (createStudent level_) names

    createStudent level_ nm = do
        logInfoN $ "Creating new student account for: " <> nm
        uname <- runDB DB.generateUsername
        logInfoN $ "Generated username is: " <> uname
        pass <- generatePassword 15
        hashedPassword <- encodePassword pass
        eName <- liftIO $ encrypt nm

        stdnt <- runDB $ DB.createStudent (eName, level_, schoolId_) (uname, hashedPassword)
        -- Name will be encrypted, so just substitute the orignal name here
        return (stdnt { name = nm } :: Student, (uname, pass))

    encodePassword p = fmap decodeUtf8 $ liftIO $ hashPassword passwordOptions (encodeUtf8 p)

    passwordOptions = defaultOptions { iterations = 3, parallelism = 1, memory = 4096}

    encryptStudent s@Student{..} = do
        eName <- liftIO $ encrypt name
        return $ (s :: Student) { name = eName }

    decryptStudent s@Student{..} = (s :: Student) { name = decrypt name }

    encrypt :: Text -> IO Text
    encrypt bytes = do
        iv :: B.ByteString <- getRandomBytes 16
        let ct = doCtr iv (encodeUtf8 bytes)
        return $ decodeUtf8 $ convertToBase Base64URLUnpadded (BA.concat [iv, ct] :: B.ByteString)

    decrypt :: Text -> Text
    decrypt txt =
        case convertFromBase Base64URLUnpadded (encodeUtf8 txt) of
            Right bytes ->
                let (iv, ct) = BA.splitAt 16 bytes
                in  decodeUtf8 $ doCtr iv ct
            Left _ -> txt

    doCtr :: B.ByteString -> B.ByteString -> B.ByteString
    doCtr ivBytes b =
        let CryptoPassed c = cipherInit key
            Just iv = makeIV ivBytes :: Maybe (IV AES256)
        in  ctrCombine c iv b

studentsServer _ _ = throwAll err403

teachersServer :: DB db => AccessScope -> SchoolId -> ApiServer TeachersApi db
teachersServer (TeacherScope _ _ (TenantKey sk) True) schoolId_ = runDB (DB.getTeachers schoolId_) :<|> activateAccount
  where
    activateAccount accountId = do
        logInfoN $ "Activating account: " <> unSubjectId accountId
        keys <- runDB (DB.getUserKeys accountId)
        case keys of
            Nothing -> logErrorN ("Activating account with no keys: " <> unSubjectId accountId) >> throwError err500
            Just ks -> do
                esk <- liftIO $ encryptSchoolKeyWithRsaKey sk (pubKey ks)
                runDB (DB.activateAccount (schoolId_, accountId) esk)

                return accountId
teachersServer _ _ = throwAll err403

answersServer :: DB db => AccessScope -> ApiServer AnswersApi db
answersServer scope = case scope of
    TeacherScope _ schoolId_ _ _ -> getAnswers schoolId_ :<|> throwAll err403
    StudentScope subId schoolId_ -> getStudentAnswers subId schoolId_  :<|> createAnswer schoolId_ subId
    _ -> throwAll err403
  where
    createAnswer schId subId a = do
        let a_ = a { studentId = subId } :: Answer
        _ <- runDB $ DB.createAnswer (a_, schId)
        return a_

    -- Student can only query their answers or answers for a specific story,
    -- not all the stories for the school, or other student's answers.
    -- So, if a specific story hasn't been requested, we ignore the extra
    -- "student" parameter value and use the student's own identity instead.
    getStudentAnswers studentSubId schoolId_ storyId_ _ =
        case storyId_ of
            Nothing -> getAnswers schoolId_ Nothing (Just studentSubId)
            sid -> getAnswers schoolId_ sid Nothing

    getAnswers schId stId subId =
        runDB (DB.getAnswers schId subId stId)

leaderBoardServer :: DB db => SchoolId -> ApiServer LeaderBoardApi db
leaderBoardServer sid =
    runDB (DB.getLeaderBoard sid)

dictServer :: DB db => ApiServer DictApi db
dictServer =
    runDB DB.getDictionary :<|> runDB . DB.lookupWord


adminServer :: DB db => ApiServer AdminApi db
adminServer (Just (AdminScope _)) = runDB DB.getDashboard
adminServer Nothing  = throwAll err401
adminServer _ = throwAll err403


-- ThrowAll idea taken from servant-auth
class ThrowAll a where
    throwAll :: ServantErr -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
    throwAll e = throwAll e :<|> throwAll e

instance {-# OVERLAPS #-} ThrowAll b => ThrowAll (a -> b) where
    throwAll e = const $ throwAll e

instance {-# OVERLAPPABLE #-} (MonadError ServantErr m) => ThrowAll (m a) where
    throwAll = throwError
