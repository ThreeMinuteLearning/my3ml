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
newLogin { sub, name, token, role } s =
    let
        userRole =
            case .userType role of
                "Teacher" ->
                    Teacher

                _ ->
                    Student
    in
        AccessToken token
            |> User name sub userRole
            |> Just
            |> Session (clearCache s.cache)


loadStories : Session -> Task Http.Error Session
loadStories session =
    let
        cache =
            session.cache
    in
        case cache.stories of
            [] ->
                Api.getStories (authorization session)
                    |> Http.toTask
                    |> Task.map (\newStories -> { session | cache = { cache | stories = newStories } })

            _ ->
                Task.succeed session


loadDictionary : Session -> Task Http.Error Session
loadDictionary session =
    let
        cache =
            session.cache
    in
        if Dict.isEmpty cache.dict then
            Api.getDictionary
                |> Http.toTask
                |> Task.map (\newDict -> { session | cache = { cache | dict = newDict } })
        else
            Task.succeed session


loadStudents : Session -> Task Http.Error Session
loadStudents session =
    let
        cache =
            session.cache
    in
        case cache.students of
            [] ->
                Api.getSchoolStudents (authorization session)
                    |> Http.toTask
                    |> Task.map (\newStudents -> { session | cache = { cache | students = newStudents } })

            _ ->
                Task.succeed session


loadClasses : Session -> Task Http.Error Session
loadClasses session =
    let
        cache =
            session.cache
    in
        case cache.classes of
            [] ->
                Api.getSchoolClasses (authorization session)
                    |> Http.toTask
                    |> Task.map (\newClasses -> { session | cache = { cache | classes = newClasses } })

            _ ->
                Task.succeed session


findStoryById : Cache -> String -> Maybe Api.Story
findStoryById cache storyId =
    firstMatch (\s -> s.id == storyId) (.stories cache)



{-
   attempt : String -> (AccessToken -> Cmd msg) -> Session -> ( List String, Cmd msg )
   attempt attemptedAction toCmd session =
       case Maybe.map .token session.user of
           Nothing ->
               [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "." ] ! []

           Just token ->
               ( [], toCmd token )
-}
