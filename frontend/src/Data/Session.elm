module Data.Session exposing (AccessToken, Session, Cache, User, Role(..), authorization, emptySession, storeSession, decodeSession, isStudent, isEditor, isTeacher, isSchoolAdmin, newLogin, loadStories, loadDictionary, loadStudents, loadUserAnswers, loadClasses, loadAnthologies, findStoryById, updateCache)

import Api
import Data.Settings as Settings exposing (Settings)
import Data.Words exposing (WordDict)
import Dict exposing (Dict)
import Exts.List exposing (firstMatch)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Ports
import Task exposing (Task)
import Util exposing ((=>))


type alias User =
    { name : String
    , sub : String
    , role : Role
    , level : Int
    , token : AccessToken
    , settings : Settings
    }


type AccessToken
    = AccessToken String


type alias Session =
    { cache : Cache
    , user : Maybe User
    }


type alias Cache =
    { dict : WordDict
    , stories : List Api.Story
    , answers : Dict Int Api.Answer
    , students : List Api.Student
    , classes : List Api.Class
    , anthologies : List Api.Anthology
    }


type Role
    = Student
    | Editor
    | Teacher Bool


emptySession : Session
emptySession =
    Session emptyCache Nothing


emptyCache : Cache
emptyCache =
    Cache Dict.empty [] Dict.empty [] [] []


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
clearCache c =
    Cache c.dict [] Dict.empty [] [] []


updateCache : (Cache -> Cache) -> Session -> Session
updateCache f session =
    { session | cache = f session.cache }


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
                |> Maybe.withDefault Settings.defaultSettings

        user =
            User name sub userRole level (AccessToken token) userSettings
    in
        Session (clearCache s.cache) (Just user)


loadStories : Session -> Task Http.Error Session
loadStories session =
    loadToCache (.stories >> List.isEmpty) Api.getStories (\newStories cache -> { cache | stories = List.reverse (List.sortBy .id newStories) }) session


loadUserAnswers : Session -> Task Http.Error Session
loadUserAnswers session =
    case Maybe.map .role session.user of
        Just Student ->
            loadToCache (.answers >> Dict.isEmpty) (\token -> Api.getSchoolAnswers token Nothing Nothing) (\newAnswers cache -> { cache | answers = answersToDict newAnswers }) session

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
loadToCache isDirty mkAuthorizedRequest updateCache session =
    let
        cache =
            session.cache
    in
        if isDirty cache then
            mkAuthorizedRequest (authorization session)
                |> Http.toTask
                |> Task.map (\a -> { session | cache = updateCache a cache })
        else
            Task.succeed session


findStoryById : Cache -> Int -> Maybe Api.Story
findStoryById cache storyId =
    firstMatch (\s -> s.id == storyId) cache.stories


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
        |> Session emptyCache


encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ "name" => Encode.string user.name
        , "sub" => Encode.string user.sub
        , "role" => encodeRole user.role
        , "level" => Encode.int user.level
        , "token" => encodeAccessToken user.token
        , "settings" => Settings.encode user.settings
        ]


userDecoder : Decoder User
userDecoder =
    decode User
        |> required "name" Decode.string
        |> required "sub" Decode.string
        |> required "role" roleDecoder
        |> required "level" Decode.int
        |> required "token" accessTokenDecoder
        |> required "settings" Settings.decoder


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
