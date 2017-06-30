module Data.Session exposing (AccessToken, Session, Cache, User, Role(..), authorization, emptySession, isStudent, newLogin, loadStories, loadDictionary, loadStudents, loadClasses, findStoryById)

import Api
import Data.Words exposing (WordDict)
import Dict
import Exts.List exposing (firstMatch)
import Http
import Task exposing (Task)


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
    Session (Cache Dict.empty [] [] []) Nothing


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


newLogin : Api.Login -> Session -> Session
newLogin { sub, name, level, token, role } s =
    let
        userRole =
            case .userType role of
                "Teacher" ->
                    Teacher

                _ ->
                    Student
    in
        AccessToken token
            |> User name sub userRole level
            |> Just
            |> Session (clearCache s.cache)


loadStories : Session -> Task Http.Error Session
loadStories =
    loadToCache (.stories >> List.isEmpty) Api.getStories (\newStories cache -> { cache | stories = newStories })


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
    firstMatch (\s -> s.id == storyId) (.stories cache)
