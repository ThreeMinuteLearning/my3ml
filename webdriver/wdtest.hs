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
import qualified Data.Text as T
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

chromiumConfig :: WDConfig
chromiumConfig =
    useBrowser chromium defaultConfig { wdHost = "0.0.0.0", wdPort = 9515, wdHTTPRetryCount = 50 }
  where
    chromium = chrome
        { chromeBinary = Just "/usr/bin/chromium"
        , chromeOptions = [ "--mute-audio"
                          , "--disable-gpu"
                          , "--no-sandbox"
                          -- , "--headless"
                          ]
        }

runTests = myRunSession defaultConfig $ do
    openPage "http://localhost:8000"
    title <- getTitle
    expect (title == "Home")
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
    loginFail "hg@mt.zoo" "wrongpassword"
    loginSucceed "hg@mt.zoo" "gobananasagain"
    -- Create student accounts
    createStudents
    changeUsernameAndPassword "Monkey 1" "monkey1" "gobananas"
    -- Register new teacher in same school and switch to this account
    createSecondTeacherAccountAndLogin

    addNewClass "Breakfast Club"
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


createSecondTeacherAccountAndLogin = do
    code <- addNewTeacher
    logout
    registerNewTeacher code "Assistant Head Gorilla" "ahg@mt.zoo" "gobananastoo" "gobananastoo"

    -- Attempt login as new teacher (fail - wrong password)
    loginFail "ahg@mt.zoo" "wrongpassword"

    -- Attempt login again as new teacher (fail - not active)
    login "ahg@mt.zoo" "gobananastoo"
    expectFormError "Please wait till your account is enabled before signing in"

    -- Log in as original teacher
    loginSucceed "hg@mt.zoo" "gobananasagain"
    -- Activate account and logout
    activateTeacherAccount "Assistant Head Gorilla"
    logout

    -- Log in as new teacher
    loginSucceed "ahg@mt.zoo" "gobananastoo"


deleteTestData = callCommand "psql my3ml -f delete_monkey_school.sql"

expectFormError :: Text -> MyWD ()
expectFormError msg = do
    elts <- findElems (ByXPath "//ul[@id='error-messages']/li")
    errors <- mapM getText elts
    expect (msg `elem` errors)

activateNewRegistrations = liftIO $ callCommand "psql my3ml -c \"UPDATE login SET active = true WHERE active = false AND user_type = 'SchoolAdmin'\""

goHome = findElem (ById "nav-home") >>= click

goTeacherAdmin = findElem (ById "nav-teacher-admin") >>= click


createStudents = do
    goTeacherAdmin
    findElem (ById "add-students-button") >>= click
    newStudentsForm <- findElem (ByTag "form")
    newStudentsTextArea <- findElemFrom newStudentsForm (ByTag "textarea")
    sendKeys "Monkey 1, Monkey 2, Monkey 3, Monkey 4, Monkey 5, Gorilla 1, Gorilla 2" newStudentsTextArea
    submit newStudentsForm
    waitForDialogToClose goHome


waitForDialogToClose action =
    -- Wait until the dialog has closed otherwise it obscures the Home link giving an UnknownError
    waitUntil 10 (catchFailedCommand UnknownError action)

findButton text =
    findElem (ByXPath (T.concat ["//button[normalize-space() = '" , text, "']"]))


changeUsernameAndPassword name newName newPassword = do
    goTeacherAdmin
    findElem (ByLinkText name) >>= click
    findButton "Change password" >>= click
    passwordInput <- findElem (ByXPath "//input[@placeholder='Password']")
    passwordConfirmInput <- findElem (ByXPath "//input[@placeholder='Confirm password']")
    sendKeys newPassword passwordInput
    sendKeys newPassword passwordConfirmInput
    findButton "Save new password" >>= click
    waitForDialogToClose goTeacherAdmin
    findElem (ByLinkText name) >>= click
    findButton "Change username" >>= click
    newUsernameInput <- findElem (ByXPath "//input[@placeholder='New username']")
    sendKeys newName newUsernameInput
    findButton "Save new username" >>= click
    waitForDialogToClose goTeacherAdmin


registerNewSchool schoolName teacherName email password confirmPassword = do
    goHome
    findElem (ByLinkText "Sign up") >>= click
    findElem (ById "register-school") >>= click
    fillInAndSubmitRegForm schoolName teacherName email password confirmPassword

fillInAndSubmitRegForm codeOrSchoolName teacherName email password confirmPassword = do
    regForm <- findElem (ByTag "form")
    regFormElts <- findElemsFrom regForm (ByTag "input")
    case regFormElts of
        [i1, i2, i3, i4, i5] -> do
            sendKeys codeOrSchoolName i1
            sendKeys teacherName i2
            sendKeys email i3
            sendKeys password i4
            sendKeys confirmPassword i5
            findElemFrom regForm (ByTag "button") >>= click

addNewTeacher = do
    goTeacherAdmin
    findElem (ById "teachers-button") >>= click
    findElem (ById "new-registration-code-button") >>= click
    findElem (ById "registration-code") >>= getText

addNewClass name = do
    goTeacherAdmin
    findElem (ById "classes-button") >>= click
    findElem (ById "add-class-button") >>= click
    newClassForm <- findElem (ByTag "form")
    classFormElts <- findElemsFrom newClassForm (ByTag "input")
    case classFormElts of
        [nameInput, descriptionInput] -> do
            sendKeys name nameInput
            sendKeys (T.concat ["Description for class ", name]) descriptionInput
            findElemFrom newClassForm (ByTag "button") >>= click

registerNewTeacher code teacherName email password confirmPassword = do
    goHome
    findElem (ByLinkText "Sign up") >>= click
    findElem (ById "register-teacher") >>= click
    fillInAndSubmitRegForm code teacherName email password confirmPassword

activateTeacherAccount teacherName = do
    goTeacherAdmin
    findElem (ById "teachers-button") >>= click
    findElem (ByXPath $ T.concat ["//table/tbody/tr/td[text()='", teacherName, "']/../td/button"]) >>= click

loginFail name pass = do
    login name pass
    waitUntil 5 (findElem (ById "error-messages"))
    expectFormError "Login failed. Check your username and password"

loginSucceed name pass = do
    login name pass
    waitUntil 5 (findElem (ById "nav-logout"))

login name pass = do
    goHome
    findElem (ById "nav-login") >>= click
    loginForm <- findElem (ByTag "form")
    loginFormElts <- findElemsFrom loginForm (ByTag "input")
    case loginFormElts of
        [nameInput, passwordInput] -> do
            sendKeys name nameInput
            sendKeys pass passwordInput
            -- submit loginForm
            findElemFrom loginForm (ByTag "button") >>= click

logout = findElem (ById "nav-logout") >>= click
