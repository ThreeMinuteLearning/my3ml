module Data.Session exposing (AccessToken, Alert(..), Cache, Role(..), Session, User, addToWorkQueue, authorization, clearWorkQueue, closeAlert, decodeSession, emptySession, error, findStoryById, isEditor, isSchoolAdmin, isStudent, isTeacher, loadAnthologies, loadClasses, loadDictionary, loadStories, loadStudents, loadUserAnswers, newLogin, saveWorkQueue, storeSession, success, updateCache, warn)

import Api
import Data.Settings as Settings exposing (Settings, defaultSettings)
import Data.Words exposing (WordDict)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder, nullable)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import List.Extra
import Ports
import Task exposing (Task)


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


type alias Session =
    { cache : Cache
    , alerts : List ( Alert, Bool )
    , workQueue : List Api.Story
    , user : Maybe User
    }


type alias Cache =
    { dict : WordDict
    , stories : List Api.Story
    , answers : Dict Int Api.Answer
    , students : List Api.Student
    , classes : List Api.Class
    , anthologies : List Api.Anthology
    , newAccounts : List ( Api.Student, ( String, String ) )
    , selectedStories : List Api.Story
    }


type Role
    = Student
    | Editor
    | Teacher Bool


type Alert
    = Success String
    | Error String
    | Warning String


emptySession : Session
emptySession =
    Session emptyCache [] [] Nothing


emptyCache : Cache
emptyCache =
    Cache Dict.empty [] Dict.empty [] [] [] [] []


hasRole : Role -> Session -> Bool
hasRole r session =
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
authorization session =
    Maybe.map .token session.user
        |> Maybe.map (\(AccessToken s) -> s)
        |> Maybe.withDefault ""


clearCache : Cache -> Cache
clearCache oldCache =
    emptyCache
        |> (\c -> { c | dict = oldCache.dict })


updateCache : (Cache -> Cache) -> Session -> Session
updateCache f session =
    { session | cache = f session.cache }


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
alert a session =
    { session | alerts = ( a, False ) :: session.alerts }


closeAlert : Alert -> Session -> Session
closeAlert a session =
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
newLogin s { sub, name, level, token, role, settings } =
    let
        userRole =
            stringToRole role.userType

        userSettings =
            settings
                |> Maybe.andThen (Result.toMaybe << Decode.decodeValue Settings.decoder)

        user =
            User name sub userRole level (AccessToken token) userSettings
    in
    Session (clearCache s.cache) [] [] (Just user)


loadStories : Session -> Task Http.Error Session
loadStories session =
    loadToCache (.stories >> List.isEmpty) Api.getStories (\newStories cache -> { cache | stories = List.reverse (List.sortBy .id newStories) }) session


populateWorkQueue : Session -> Task e Session
populateWorkQueue sesh =
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
    Task.succeed { sesh | workQueue = workQueue }


addToWorkQueue : List Api.Story -> Session -> Session
addToWorkQueue stories session =
    let
        newQueue =
            List.Extra.uniqueBy .id (List.append session.workQueue stories)
    in
    { session | workQueue = newQueue }


saveWorkQueue : (Result Http.Error Api.NoContent -> msg) -> Session -> ( Cmd msg, Session )
saveWorkQueue toMsg session =
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
            { session | user = newUser }

        save =
            Cmd.batch
                [ storeSession newSession
                , Api.postAccountSettings (authorization session) (Settings.encode newSettings)
                    |> Http.send toMsg
                ]
    in
    ( save, newSession )


clearWorkQueue : Session -> Session
clearWorkQueue session =
    { session | workQueue = [] }


loadUserAnswers : Session -> Task Http.Error Session
loadUserAnswers session =
    case Maybe.map .role session.user of
        Just Student ->
            loadToCache (.answers >> Dict.isEmpty) (\token -> Api.getSchoolAnswers token Nothing Nothing) (\newAnswers cache -> { cache | answers = answersToDict newAnswers }) session
                |> Task.andThen populateWorkQueue

        _ ->
            Task.succeed session


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
loadToCache isDirty mkAuthorizedRequest mkCache session =
    let
        cache =
            session.cache
    in
    if isDirty cache then
        mkAuthorizedRequest (authorization session)
            |> Http.toTask
            |> Task.map (\a -> { session | cache = mkCache a cache })

    else
        Task.succeed session


findStoryById : Cache -> Int -> Maybe Api.Story
findStoryById cache storyId =
    List.Extra.find (\s -> s.id == storyId) cache.stories


storeSession : Session -> Cmd msg
storeSession session =
    Maybe.map encodeUser session.user
        |> Maybe.withDefault (Encode.string "")
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession



-- Encoding and Decoding


decodeSession : Decode.Value -> Session
decodeSession json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString userDecoder >> Result.toMaybe)
        |> Session emptyCache [] []


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
