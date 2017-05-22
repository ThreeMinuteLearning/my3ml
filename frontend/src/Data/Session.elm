module Data.Session exposing (AccessToken, Session, User, Role(..), authorization, isStudent, newLogin, loadStories, loadDictionary)

import Api
import Data.Words exposing (WordDict)
import Dict
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
    { dict : WordDict
    , stories : List Api.Story
    , user : Maybe User
    }


type Role
    = Student
    | Teacher


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
            |> Session s.dict []


loadStories : Session -> Task Http.Error Session
loadStories session =
    let
        load =
            case session.stories of
                [] ->
                    Api.getStories (authorization session)
                        |> Http.toTask

                _ ->
                    Task.succeed session.stories
    in
        Task.map (\newStories -> { session | stories = newStories }) load


loadDictionary : Session -> Task Http.Error Session
loadDictionary session =
    let
        load =
            if Dict.isEmpty session.dict then
                Api.getDictionary
                    |> Http.toTask
            else
                Task.succeed session.dict
    in
        Task.map (\newDict -> { session | dict = newDict }) load



{-
   attempt : String -> (AccessToken -> Cmd msg) -> Session -> ( List String, Cmd msg )
   attempt attemptedAction toCmd session =
       case Maybe.map .token session.user of
           Nothing ->
               [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "." ] ! []

           Just token ->
               ( [], toCmd token )
-}
