{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving #-}

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl(..), StM)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Exception (finally)
import Control.Exception.Base (fromException, SomeException(..))
import Control.Exception.Lifted (throwIO, catch)
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Aeson
import Data.Text (Text)
import Test.WebDriver
import Test.WebDriver.Class (WebDriver(..), Method)
import Test.WebDriver.Internal (mkRequest, sendHTTPRequest, getJSONResult)
import Test.WebDriver.Config (WebDriverConfig(..))
import Test.WebDriver.Session (WDSession, WDSessionState)
import Test.WebDriver.Commands.Wait
import Control.Concurrent
import System.Process

newtype MyWD a =  MyWD (WD a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadFix, WDSessionState)

instance MonadBase IO MyWD where
    liftBase = MyWD . liftBase

instance MonadBaseControl IO MyWD where
    type StM MyWD a = StM (StateT WDSession IO) a

    liftBaseWith f = MyWD . WD $
        liftBaseWith $ \runInBase -> f (\(MyWD (WD st)) -> runInBase st)

    restoreM = MyWD . restoreM

instance WebDriver MyWD where
    doCommand method path args = do
        result <- myDoCommand method path args
        pause 500
        case result of
           Left e -> case fromException e of
               Just (FailedCommand StaleElementReference _) ->
                   -- This won't actually work since this is probably a command using the stale reference
                   -- and the find command would need to be repeated
                   myDoCommand method path args >>= either throwIO return
               _ -> throwIO e
           Right yay -> return yay

myDoCommand :: (ToJSON a, FromJSON b) => Method -> Text -> a -> MyWD (Either SomeException b)
myDoCommand method path args =
    mkRequest method path args
    >>= sendHTTPRequest
    >>= either throwIO return
    >>= getJSONResult

myRunSession :: WebDriverConfig conf => conf -> MyWD a -> IO a
myRunSession conf (MyWD wd) = do
  sess <- mkSession conf
  caps <- mkCaps conf
  runWD sess $ createSession caps >> wd

main = finally runTests deleteTestData

runTests = myRunSession defaultConfig $ do
    openPage "http://localhost:8000"
    title <- getTitle
    expect (title == "3ml")
    setImplicitWait 5000
    smokeTests
    closeSession

-- Pause in milliseconds
pause n = liftIO (threadDelay (n*1000))

smokeTests :: MyWD ()
smokeTests = do
    -- Anonymous User

    -- View story
    sampleStories <- findElem (ById "storytiles") >>= \elt -> findElemsFrom elt (ByTag "a")
    click (head sampleStories)
    goHome
    -- Count sample story links
    -- Check menu items


    -- Teacher stuff

    -- Register new school account
    registerNewSchool "Monkey Test School" "Head Gorilla" "hg@mt.zoo" "gobananasagain" "gobananasagain"
    -- Activate account (call psql)
    activateNewRegistrations
    -- Log in as registered teacher
    login "hg@mt.zoo" "gobananasagain"
    -- Create student accounts
    createStudents
    -- Register new teacher in same school
    code <- addNewTeacher
    logout
    registerNewTeacher code "Assistant Head Gorilla" "ahg@mt.zoo" "gobananasagain" "gobananasagain"

    -- Attempt login as new teacher (fail - wrong password)
    login "ahg@mt.zoo" "gobananas"
    expectFormError "Username or password is incorrect"

    -- Attempt login again as new teacher (fail - not active)
    login "ahg@mt.zoo" "gobananasagain"
    expectFormError "Please wait till your account is enabled before signing in"
    -- Log in as original teacher

    login "hg@mt.zoo" "gobananasagain"
    -- Activate account and logout

    -- Log in as new teacher
    -- Create class and add students to it
    -- Find a story
    --    Search for something
    --    Enter browser
    --    Select stories
    --    Return to stories
    --    Create anthology from basket
    --    View anthologies
    --    Check count of stories in new anthology
    -- Log out

    logout

    -- Student stuff

    -- Log in
    -- Find a story
    -- Complete story
    -- View leaderboard


deleteTestData = callCommand "psql my3ml -f delete_monkey_school.sql"

expectFormError :: Text -> MyWD ()
expectFormError msg = do
    elts <- findElems (ByXPath "//ul[@class='error-messages']/li")
    errors <- mapM getText elts
    expect (msg `elem` errors)

activateNewRegistrations = liftIO $ callCommand "psql my3ml -c \"UPDATE login SET active = true WHERE active = false AND user_type = 'SchoolAdmin'\""

goHome = findElem (ByLinkText "Home") >>= click


createStudents = do
    findElem (ByLinkText "Teacher") >>= click
    findElem (ById "add-students-button") >>= click
    newStudentsForm <- findElem (ByTag "form")
    newStudentsTextArea <- findElemFrom newStudentsForm (ByTag "textarea")
    sendKeys "Monkey 1, Monkey 2, Monkey 3, Monkey 4, Monkey 5, Gorilla 1, Gorilla 2" newStudentsTextArea
    submit newStudentsForm
    -- Wait until the dialog has closed otherwise it obscures the Home link giving an UnknownError
    waitUntil 10 (catchFailedCommand UnknownError goHome)


registerNewSchool schoolName teacherName email password confirmPassword = do
    goHome
    findElem (ByLinkText "Sign up") >>= click
    findElem (ById "register-school") >>= click
    fillInAndSubmitRegForm schoolName teacherName email password confirmPassword

fillInAndSubmitRegForm codeOrSchoolName teacherName email password confirmPassword = do
    regForm <- findElem (ByTag "form")
    [i1, i2, i3, i4, i5] <- findElemsFrom regForm (ByTag "input")
    sendKeys codeOrSchoolName i1
    sendKeys teacherName i2
    sendKeys email i3
    sendKeys password i4
    sendKeys confirmPassword i5
    findElemFrom regForm (ByTag "button") >>= click

addNewTeacher = do
    goHome
    findElem (ByLinkText "Teacher") >>= click
    findElem (ById "teachers-button") >>= click
    findElem (ById "new-registration-code-button") >>= click
    findElem (ByXPath "//div[contains(@class, 'registration-code')]/p") >>= getText

registerNewTeacher code teacherName email password confirmPassword = do
    goHome
    findElem (ByLinkText "Sign up") >>= click
    findElem (ById "register-teacher") >>= click
    fillInAndSubmitRegForm code teacherName email password confirmPassword

login name pass = do
    goHome
    findElem (ByLinkText "Sign in") >>= click
    loginForm <- findElem (ByTag "form")
    [nameInput, passwordInput] <- findElemsFrom loginForm (ByTag "input")
    sendKeys name nameInput
    sendKeys pass passwordInput
    -- submit loginForm
    findElemFrom loginForm (ByTag "button") >>= click

logout = findElem (ByLinkText "Sign out") >>= click
