module Data.Session exposing (AccessToken, Alert(..), Session, addToWorkQueue, authorization, clearWorkQueue, closeAlert, currentTime, decodeSession, error, findStoryById, getAlerts, getCache, getSettings, getWorkQueue, isEditor, isSchoolAdmin, isStudent, isTeacher, loadAnthologies, loadClasses, loadDictionary, loadStories, loadStudents, loadUserAnswers, logout, newLogin, saveWorkQueue, storeSession, storyCompleted, subjectId, success, tick, updateCache, updateSettings, userAgent, userLevel, warn, workQueueHasSpace)

import Api
import Cache exposing (..)
import Data.Settings as Settings exposing (Settings, defaultSettings)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder, nullable)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import List.Extra
import Ports
import StoryGraph
import Task exposing (Task)
import Time
import Tuple exposing (second)


type alias User =
    { name : String
    , sub : String
    , role : Role
    , level : Int
    , token : AccessToken
    , settings : Maybe Settings
    }


type AccessToken
    = AccessToken String


type Session
    = Session
        { cache : Cache
        , alerts : List ( Alert, Bool )
        , workQueue : List Api.Story
        , user : Maybe User
        , tick : Time.Posix
        , ua : String
        }


type Role
    = Student
    | Editor
    | Teacher Bool


type Alert
    = Success String
    | Error String
    | Warning String


userAgent : Session -> String
userAgent (Session s) =
    s.ua


currentTime : Session -> Time.Posix
currentTime (Session s) =
    s.tick


getCache : Session -> Cache
getCache (Session s) =
    s.cache


getSettings : Session -> Maybe Settings
getSettings (Session session) =
    Maybe.andThen .settings session.user


getWorkQueue : Session -> List Api.Story
getWorkQueue (Session session) =
    session.workQueue


getAlerts : Session -> List ( Alert, Bool )
getAlerts (Session session) =
    session.alerts


userLevel : Session -> Maybe Int
userLevel (Session session) =
    Maybe.map .level session.user


subjectId : Session -> Maybe String
subjectId (Session session) =
    Maybe.map .sub session.user


storyCompleted : Session -> Api.Answer -> Session
storyCompleted (Session session) answer =
    let
        cache =
            session.cache

        newWorkQueue =
            List.filter (\s -> s.id /= answer.storyId) session.workQueue

        newCache =
            { cache | answers = Dict.insert answer.storyId answer cache.answers }
    in
    Session { session | cache = newCache, workQueue = newWorkQueue }


updateSettings : Session -> Settings -> Session
updateSettings ((Session session) as sesh) newSettings =
    session.user
        |> Maybe.map (\u -> { u | settings = Just newSettings })
        |> Maybe.map (\u -> Session { session | user = Just u })
        |> Maybe.withDefault sesh


logout : Session -> Session
logout (Session session) =
    Session { session | user = Nothing, workQueue = [], cache = clearCache session.cache }


tick : Session -> Time.Posix -> Session
tick (Session session) t =
    let
        interval =
            Time.posixToMillis t - Time.posixToMillis session.tick

        alerts =
            List.filter (not << second) session.alerts

        ( newAlerts, newTick ) =
            if interval < 3000 then
                ( alerts, session.tick )

            else
                ( List.map closeUnlessError alerts, t )

        closeUnlessError ( a, c ) =
            case a of
                Success _ ->
                    ( a, True )

                _ ->
                    ( a, c )
    in
    Session { session | tick = newTick, alerts = newAlerts }


hasRole : Role -> Session -> Bool
hasRole r (Session session) =
    Maybe.map .role session.user
        |> Maybe.map ((==) r)
        |> Maybe.withDefault False


isStudent : Session -> Bool
isStudent =
    hasRole Student


isEditor : Session -> Bool
isEditor =
    hasRole Editor


isTeacher : Session -> Bool
isTeacher session =
    hasRole (Teacher True) session || hasRole (Teacher False) session


isSchoolAdmin : Session -> Bool
isSchoolAdmin =
    hasRole (Teacher True)


authorization : Session -> String
authorization (Session session) =
    Maybe.map .token session.user
        |> Maybe.map (\(AccessToken s) -> s)
        |> Maybe.withDefault ""


updateCache : (Cache -> Cache) -> Session -> Session
updateCache f (Session session) =
    Session { session | cache = f session.cache }


success : String -> Session -> Session
success =
    Success >> alert


error : String -> Session -> Session
error =
    Error >> alert


warn : String -> Session -> Session
warn =
    Warning >> alert


alert : Alert -> Session -> Session
alert a (Session session) =
    Session { session | alerts = ( a, False ) :: session.alerts }


closeAlert : Alert -> Session -> Session
closeAlert a (Session session) =
    Session
        { session
            | alerts =
                List.map
                    (\( a_, closed ) ->
                        if a_ == a then
                            ( a_, True )

                        else
                            ( a_, closed )
                    )
                    session.alerts
        }


stringToRole : String -> Role
stringToRole s =
    case s of
        "Teacher" ->
            Teacher False

        "SchoolAdmin" ->
            Teacher True

        "Editor" ->
            Editor

        _ ->
            Student


newLogin : Session -> Api.Login -> Session
newLogin (Session s) { sub, name, level, token, role, settings } =
    let
        userRole =
            stringToRole role.userType

        userSettings =
            settings
                |> Maybe.andThen (Result.toMaybe << Decode.decodeValue Settings.decoder)

        user =
            User name sub userRole level (AccessToken token) userSettings
    in
    Session { cache = clearCache s.cache, alerts = [], workQueue = [], user = Just user, tick = s.tick, ua = s.ua }


loadStories : Session -> Task Http.Error Session
loadStories session =
    loadToCache (.stories >> List.isEmpty) Api.getStories (\newStories cache -> { cache | stories = List.reverse (List.sortBy .id newStories.stories), storyGraph = StoryGraph.fromStoriesAndConnections newStories.stories newStories.graph }) session


populateWorkQueue : Session -> Task e Session
populateWorkQueue (Session sesh) =
    let
        answers =
            sesh.cache.answers

        workQueue =
            Maybe.andThen .settings sesh.user
                |> Maybe.map .workQueue
                |> Maybe.withDefault []
                |> List.filter (\id -> not (Dict.member id answers))
                |> List.filterMap (findStoryById sesh.cache)
    in
    Task.succeed (Session { sesh | workQueue = workQueue })


workQueueHasSpace : Session -> Bool
workQueueHasSpace (Session s) =
    List.length s.workQueue < 30


addToWorkQueue : List Api.Story -> Session -> Session
addToWorkQueue stories (Session session) =
    let
        newQueue =
            List.Extra.uniqueBy .id (List.append session.workQueue stories)
    in
    Session { session | workQueue = newQueue }


saveWorkQueue : (Result Http.Error Api.NoContent -> msg) -> Session -> ( Cmd msg, Session )
saveWorkQueue toMsg ((Session session) as sesh) =
    let
        newSettings =
            session.user
                |> Maybe.andThen .settings
                |> Maybe.withDefault defaultSettings
                |> (\s -> { s | workQueue = List.map .id session.workQueue })

        newUser =
            session.user
                |> Maybe.map (\u -> { u | settings = Just newSettings })

        newSession =
            Session { session | user = newUser }

        save =
            Cmd.batch
                [ storeSession newSession
                , Api.postAccountSettings (authorization sesh) (Settings.encode newSettings)
                    |> Http.send toMsg
                ]
    in
    ( save, newSession )


clearWorkQueue : Session -> Session
clearWorkQueue (Session session) =
    Session { session | workQueue = [] }


loadUserAnswers : Session -> Task Http.Error Session
loadUserAnswers ((Session session) as sesh) =
    case Maybe.map .role session.user of
        Just Student ->
            loadToCache (.answers >> Dict.isEmpty) (\token -> Api.getSchoolAnswers token Nothing Nothing) (\newAnswers cache -> { cache | answers = answersToDict newAnswers }) sesh
                |> Task.andThen
                    (\((Session s) as newSession) ->
                        if List.isEmpty s.workQueue then
                            populateWorkQueue newSession

                        else
                            Task.succeed newSession
                    )

        _ ->
            Task.succeed sesh


answersToDict : List Api.Answer -> Dict Int Api.Answer
answersToDict =
    Dict.fromList << List.map (\a -> ( a.storyId, a ))


loadDictionary : Session -> Task Http.Error Session
loadDictionary =
    loadToCache (.dict >> Dict.isEmpty) (\_ -> Api.getDictionary) (\newDict cache -> { cache | dict = newDict })


loadStudents : Session -> Task Http.Error Session
loadStudents =
    loadToCache (.students >> List.isEmpty) Api.getSchoolStudents (\newStudents cache -> { cache | students = newStudents })


loadClasses : Session -> Task Http.Error Session
loadClasses =
    loadToCache (.classes >> List.isEmpty) Api.getSchoolClasses (\newClasses cache -> { cache | classes = newClasses })


loadAnthologies : Session -> Task Http.Error Session
loadAnthologies =
    loadToCache (.anthologies >> List.isEmpty) Api.getAnthologies (\newAnthologies cache -> { cache | anthologies = newAnthologies })


loadToCache : (Cache -> Bool) -> (String -> Http.Request a) -> (a -> Cache -> Cache) -> Session -> Task Http.Error Session
loadToCache isDirty mkAuthorizedRequest mkCache ((Session session) as sesh) =
    let
        cache =
            session.cache
    in
    if isDirty cache then
        mkAuthorizedRequest (authorization sesh)
            |> Http.toTask
            |> Task.map (\a -> Session { session | cache = mkCache a cache })

    else
        Task.succeed (Session session)


findStoryById : Cache -> Int -> Maybe Api.Story
findStoryById cache storyId =
    List.Extra.find (\s -> s.id == storyId) cache.stories


storeSession : Session -> Cmd msg
storeSession (Session session) =
    Maybe.map encodeUser session.user
        |> Maybe.withDefault (Encode.string "")
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession



-- Encoding and Decoding


decodeSession : Decode.Value -> Session
decodeSession json =
    let
        decoder =
            Decode.succeed mkSession
                |> required "ua" Decode.string
                |> optional "session" Decode.string ""

        mkSession ua session =
            let
                user =
                    Decode.decodeString userDecoder session
                        |> Result.toMaybe
            in
            Session { cache = emptyCache, alerts = [], workQueue = [], user = user, tick = Time.millisToPosix 0, ua = ua }
    in
    Decode.decodeValue decoder json
        |> Result.toMaybe
        |> Maybe.withDefault (mkSession "" "")


encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "name", Encode.string user.name )
        , ( "sub", Encode.string user.sub )
        , ( "role", encodeRole user.role )
        , ( "level", Encode.int user.level )
        , ( "token", encodeAccessToken user.token )
        , ( "settings", Maybe.map Settings.encode user.settings |> Maybe.withDefault Encode.null )
        ]


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "name" Decode.string
        |> required "sub" Decode.string
        |> required "role" roleDecoder
        |> required "level" Decode.int
        |> required "token" accessTokenDecoder
        |> required "settings" (nullable Settings.decoder)


encodeRole : Role -> Encode.Value
encodeRole r =
    Encode.string <|
        case r of
            Student ->
                "Student"

            Teacher False ->
                "Teacher"

            Teacher True ->
                "SchoolAdmin"

            Editor ->
                "Editor"


roleDecoder : Decoder Role
roleDecoder =
    Decode.map stringToRole Decode.string


encodeAccessToken : AccessToken -> Encode.Value
encodeAccessToken (AccessToken t) =
    Encode.string t


accessTokenDecoder : Decoder AccessToken
accessTokenDecoder =
    Decode.map AccessToken Decode.string
