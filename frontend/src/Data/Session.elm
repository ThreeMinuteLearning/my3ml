module Data.Session exposing (AccessToken, Session, Cache, User, Role(..), authorization, emptySession, storeSession, decodeSession, isStudent, newLogin, loadStories, loadDictionary, loadStudents, loadClasses, findStoryById)

import Api
import Data.Words exposing (WordDict)
import Dict
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
    , students : List Api.Student
    , classes : List Api.Class
    }


type Role
    = Student
    | Teacher


emptySession : Session
emptySession =
    Session emptyCache Nothing


emptyCache : Cache
emptyCache =
    Cache Dict.empty [] [] []


isStudent : Session -> Bool
isStudent session =
    case Maybe.map .role session.user of
        Just Student ->
            True

        _ ->
            False


authorization : Session -> String
authorization session =
    Maybe.map .token session.user
        |> Maybe.map (\(AccessToken s) -> s)
        |> Maybe.withDefault ""


clearCache : Cache -> Cache
clearCache c =
    Cache c.dict [] [] []


stringToRole : String -> Role
stringToRole s =
    case s of
        "Teacher" ->
            Teacher

        _ ->
            Student


newLogin : Session -> Api.Login -> Session
newLogin s { sub, name, level, token, role } =
    let
        userRole =
            stringToRole role.userType
    in
        AccessToken token
            |> User name sub userRole level
            |> Just
            |> Session (clearCache s.cache)


loadStories : Session -> Task Http.Error Session
loadStories session =
    loadToCache (.stories >> List.isEmpty) Api.getStories (\newStories cache -> { cache | stories = organizeStories session.user newStories }) session


organizeStories : Maybe User -> List Api.Story -> List Api.Story
organizeStories user stories =
    case user of
        Nothing ->
            stories

        Just u ->
            if u.role == Student then
                sortForLevel u.level stories
            else
                stories


sortForLevel : Int -> List Api.Story -> List Api.Story
sortForLevel l stories =
    List.sortBy (\s -> abs (s.level - l)) stories


loadDictionary : Session -> Task Http.Error Session
loadDictionary =
    loadToCache (.dict >> Dict.isEmpty) (\_ -> Api.getDictionary) (\newDict cache -> { cache | dict = newDict })


loadStudents : Session -> Task Http.Error Session
loadStudents =
    loadToCache (.students >> List.isEmpty) Api.getSchoolStudents (\newStudents cache -> { cache | students = newStudents })


loadClasses : Session -> Task Http.Error Session
loadClasses =
    loadToCache (.classes >> List.isEmpty) Api.getSchoolClasses (\newClasses cache -> { cache | classes = newClasses })


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


findStoryById : Cache -> String -> Maybe Api.Story
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
        ]


userDecoder : Decoder User
userDecoder =
    decode User
        |> required "name" Decode.string
        |> required "sub" Decode.string
        |> required "role" roleDecoder
        |> required "level" Decode.int
        |> required "token" accessTokenDecoder


encodeRole : Role -> Encode.Value
encodeRole r =
    Encode.string <|
        case r of
            Student ->
                "Student"

            Teacher ->
                "Teacher"


roleDecoder : Decoder Role
roleDecoder =
    Decode.map stringToRole Decode.string


encodeAccessToken : AccessToken -> Encode.Value
encodeAccessToken (AccessToken t) =
    Encode.string t


accessTokenDecoder : Decoder AccessToken
accessTokenDecoder =
    Decode.map AccessToken Decode.string
