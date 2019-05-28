module Api exposing (..)

import Dict exposing (Dict)

import Tuple

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Url


type alias Story =
    { id : Int
    , title : String
    , img : String
    , level : Int
    , qualification : Maybe (String)
    , curriculum : Maybe (String)
    , tags : List (String)
    , content : String
    , words : List (DictEntry)
    , clarifyWord : String
    , enabled : Bool
    , createdAt : Int
    }

type alias DictEntry =
    { word : String
    , index : Int
    }

type alias School =
    { id : String
    , name : String
    , description : Maybe (String)
    }

type alias Answer =
    { storyId : Int
    , studentId : String
    , connect : String
    , question : String
    , summarise : String
    , clarify : String
    }

type alias Class =
    { id : String
    , name : String
    , description : Maybe (String)
    , schoolId : String
    , createdBy : String
    , students : List (String)
    }

type alias Login =
    { sub : String
    , username : String
    , name : String
    , role : UserType
    , level : Int
    , settings : Maybe (Value)
    , token : String
    }

type alias UserType =
    { userType : String
    }

type alias Student =
    { id : String
    , name : String
    , description : Maybe (String)
    , level : Int
    , schoolId : String
    , hidden : Bool
    , deleted : Maybe (Float)
    }

type alias Teacher =
    { id : String
    , name : String
    , bio : Maybe (String)
    , schoolId : String
    }

type alias Anthology =
    { id : String
    , name : String
    , description : String
    , createdBy : String
    , schoolId : Maybe (String)
    , stories : List (Int)
    , hidden : Bool
    }

type alias LoginRequest =
    { username : String
    , password : String
    , otp : Maybe (Int)
    , ua : String
    }

type alias LeaderBoardEntry =
    { position : Int
    , name : String
    , studentId : String
    , score : Int
    }

type alias Registration =
    { email : String
    , code : Maybe (String)
    , schoolName : String
    , teacherName : String
    , password : String
    }

type NoContent
    = NoContent

getStories : String -> Http.Request (List (Story))
getStories header_Authorization =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "stories"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeStory)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getStoriesByStoryId : String -> Int -> Http.Request (Story)
getStoriesByStoryId header_Authorization capture_storyId =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "stories"
                , capture_storyId |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeStory
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postStoriesByStoryId : String -> Int -> Story -> Http.Request (Story)
postStoriesByStoryId header_Authorization capture_storyId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "stories"
                , capture_storyId |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.jsonBody (encodeStory body)
        , expect =
            Http.expectJson decodeStory
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postStories : String -> Story -> Http.Request (Story)
postStories header_Authorization body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "stories"
                ]
        , body =
            Http.jsonBody (encodeStory body)
        , expect =
            Http.expectJson decodeStory
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getDictionary : Http.Request (Dict (String) (List ((String, List ((String, Int))))))
getDictionary =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "/api"
                , "dictionary"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (dict (list (map2 Tuple.pair (index 0 string) (index 1 (list (map2 Tuple.pair (index 0 string) (index 1 int)))))))
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getDictionaryByWord : String -> Http.Request (List ((String, List ((String, Int)))))
getDictionaryByWord capture_word =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "/api"
                , "dictionary"
                , capture_word |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map2 Tuple.pair (index 0 string) (index 1 (list (map2 Tuple.pair (index 0 string) (index 1 int))))))
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchools : String -> Http.Request (List (School))
getSchools header_Authorization =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeSchool)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolsBySchoolIdClasses : String -> String -> Http.Request (List (Class))
getSchoolsBySchoolIdClasses header_Authorization capture_schoolId =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "classes"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeClass)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolsBySchoolIdClassesByClassId : String -> String -> String -> Http.Request (Class)
getSchoolsBySchoolIdClassesByClassId header_Authorization capture_schoolId capture_classId =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "classes"
                , capture_classId |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeClass
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteSchoolsBySchoolIdClassesByClassId : String -> String -> String -> Http.Request (NoContent)
deleteSchoolsBySchoolIdClassesByClassId header_Authorization capture_schoolId capture_classId =
    Http.request
        { method =
            "DELETE"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "classes"
                , capture_classId |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolsBySchoolIdClassesByClassIdMembers : String -> String -> String -> Maybe (Bool) -> List (String) -> Http.Request (Class)
postSchoolsBySchoolIdClassesByClassIdMembers header_Authorization capture_schoolId capture_classId query_delete body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_delete
                    |> Maybe.map ((\v -> if v then "True" else "False") >> Url.percentEncode >> (++) "delete=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                    ]
            , url =
                String.join "/"
                    [ "/api"
                    , "schools"
                    , capture_schoolId |> Url.percentEncode
                    , "classes"
                    , capture_classId |> Url.percentEncode
                    , "members"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.jsonBody ((Json.Encode.list  Json.Encode.string) body)
            , expect =
                Http.expectJson decodeClass
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postSchoolsBySchoolIdClasses : String -> String -> (String, String) -> Http.Request (Class)
postSchoolsBySchoolIdClasses header_Authorization capture_schoolId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "classes"
                ]
        , body =
            Http.jsonBody ((\(x, y) -> Json.Encode.list identity [ Json.Encode.string x, Json.Encode.string y ]) body)
        , expect =
            Http.expectJson decodeClass
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolsBySchoolIdStudents : String -> String -> Http.Request (List (Student))
getSchoolsBySchoolIdStudents header_Authorization capture_schoolId =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "students"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeStudent)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolsBySchoolIdStudentsByStudentId : String -> String -> String -> Http.Request (Student)
getSchoolsBySchoolIdStudentsByStudentId header_Authorization capture_schoolId capture_studentId =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "students"
                , capture_studentId |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeStudent
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolsBySchoolIdStudentsByStudentId : String -> String -> String -> Student -> Http.Request (Student)
postSchoolsBySchoolIdStudentsByStudentId header_Authorization capture_schoolId capture_studentId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "students"
                , capture_studentId |> Url.percentEncode
                ]
        , body =
            Http.jsonBody (encodeStudent body)
        , expect =
            Http.expectJson decodeStudent
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolsBySchoolIdStudentsByStudentIdPassword : String -> String -> String -> String -> Http.Request (NoContent)
postSchoolsBySchoolIdStudentsByStudentIdPassword header_Authorization capture_schoolId capture_studentId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "students"
                , capture_studentId |> Url.percentEncode
                , "password"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolsBySchoolIdStudentsByStudentIdUsername : String -> String -> String -> String -> Http.Request (NoContent)
postSchoolsBySchoolIdStudentsByStudentIdUsername header_Authorization capture_schoolId capture_studentId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "students"
                , capture_studentId |> Url.percentEncode
                , "username"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteSchoolsBySchoolIdStudentsByStudentId : String -> String -> String -> Http.Request (Student)
deleteSchoolsBySchoolIdStudentsByStudentId header_Authorization capture_schoolId capture_studentId =
    Http.request
        { method =
            "DELETE"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "students"
                , capture_studentId |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeStudent
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolsBySchoolIdStudentsByStudentIdUndelete : String -> String -> String -> Http.Request (NoContent)
postSchoolsBySchoolIdStudentsByStudentIdUndelete header_Authorization capture_schoolId capture_studentId =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "students"
                , capture_studentId |> Url.percentEncode
                , "undelete"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolsBySchoolIdStudents : String -> String -> (Int, List (String)) -> Http.Request (List ((Student, (String, String))))
postSchoolsBySchoolIdStudents header_Authorization capture_schoolId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "students"
                ]
        , body =
            Http.jsonBody ((\(x, y) -> Json.Encode.list identity [ Json.Encode.int x, (Json.Encode.list  Json.Encode.string) y ]) body)
        , expect =
            Http.expectJson (list (map2 Tuple.pair (index 0 decodeStudent) (index 1 (map2 Tuple.pair (index 0 string) (index 1 string)))))
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolsBySchoolIdAnswers : String -> String -> Maybe (Int) -> Maybe (String) -> Http.Request (List (Answer))
getSchoolsBySchoolIdAnswers header_Authorization capture_schoolId query_story query_student =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_story
                    |> Maybe.map (String.fromInt >> Url.percentEncode >> (++) "story=")
                    |> Maybe.withDefault ""
                , query_student
                    |> Maybe.map (identity >> Url.percentEncode >> (++) "student=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                    ]
            , url =
                String.join "/"
                    [ "/api"
                    , "schools"
                    , capture_schoolId |> Url.percentEncode
                    , "answers"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson (list decodeAnswer)
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postSchoolsBySchoolIdAnswers : String -> String -> Answer -> Http.Request (Answer)
postSchoolsBySchoolIdAnswers header_Authorization capture_schoolId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "answers"
                ]
        , body =
            Http.jsonBody (encodeAnswer body)
        , expect =
            Http.expectJson decodeAnswer
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolsBySchoolIdLeaderboard : String -> String -> Http.Request (List (LeaderBoardEntry))
getSchoolsBySchoolIdLeaderboard header_Authorization capture_schoolId =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "leaderboard"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeLeaderBoardEntry)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolsBySchoolIdTeachers : String -> String -> Http.Request (List ((Teacher, Bool)))
getSchoolsBySchoolIdTeachers header_Authorization capture_schoolId =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "teachers"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map2 Tuple.pair (index 0 decodeTeacher) (index 1 bool)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolsBySchoolIdTeachersByTeacherIdActivate : String -> String -> String -> Http.Request (String)
postSchoolsBySchoolIdTeachersByTeacherIdActivate header_Authorization capture_schoolId capture_teacherId =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "schools"
                , capture_schoolId |> Url.percentEncode
                , "teachers"
                , capture_teacherId |> Url.percentEncode
                , "activate"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolClasses : String -> Http.Request (List (Class))
getSchoolClasses header_Authorization =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "classes"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeClass)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolClassesByClassId : String -> String -> Http.Request (Class)
getSchoolClassesByClassId header_Authorization capture_classId =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "classes"
                , capture_classId |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeClass
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteSchoolClassesByClassId : String -> String -> Http.Request (NoContent)
deleteSchoolClassesByClassId header_Authorization capture_classId =
    Http.request
        { method =
            "DELETE"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "classes"
                , capture_classId |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolClassesByClassIdMembers : String -> String -> Maybe (Bool) -> List (String) -> Http.Request (Class)
postSchoolClassesByClassIdMembers header_Authorization capture_classId query_delete body =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_delete
                    |> Maybe.map ((\v -> if v then "True" else "False") >> Url.percentEncode >> (++) "delete=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                    ]
            , url =
                String.join "/"
                    [ "/api"
                    , "school"
                    , "classes"
                    , capture_classId |> Url.percentEncode
                    , "members"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.jsonBody ((Json.Encode.list  Json.Encode.string) body)
            , expect =
                Http.expectJson decodeClass
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postSchoolClasses : String -> (String, String) -> Http.Request (Class)
postSchoolClasses header_Authorization body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "classes"
                ]
        , body =
            Http.jsonBody ((\(x, y) -> Json.Encode.list identity [ Json.Encode.string x, Json.Encode.string y ]) body)
        , expect =
            Http.expectJson decodeClass
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolStudents : String -> Http.Request (List (Student))
getSchoolStudents header_Authorization =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "students"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeStudent)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolStudentsByStudentId : String -> String -> Http.Request (Student)
getSchoolStudentsByStudentId header_Authorization capture_studentId =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "students"
                , capture_studentId |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeStudent
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolStudentsByStudentId : String -> String -> Student -> Http.Request (Student)
postSchoolStudentsByStudentId header_Authorization capture_studentId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "students"
                , capture_studentId |> Url.percentEncode
                ]
        , body =
            Http.jsonBody (encodeStudent body)
        , expect =
            Http.expectJson decodeStudent
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolStudentsByStudentIdPassword : String -> String -> String -> Http.Request (NoContent)
postSchoolStudentsByStudentIdPassword header_Authorization capture_studentId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "students"
                , capture_studentId |> Url.percentEncode
                , "password"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolStudentsByStudentIdUsername : String -> String -> String -> Http.Request (NoContent)
postSchoolStudentsByStudentIdUsername header_Authorization capture_studentId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "students"
                , capture_studentId |> Url.percentEncode
                , "username"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteSchoolStudentsByStudentId : String -> String -> Http.Request (Student)
deleteSchoolStudentsByStudentId header_Authorization capture_studentId =
    Http.request
        { method =
            "DELETE"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "students"
                , capture_studentId |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeStudent
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolStudentsByStudentIdUndelete : String -> String -> Http.Request (NoContent)
postSchoolStudentsByStudentIdUndelete header_Authorization capture_studentId =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "students"
                , capture_studentId |> Url.percentEncode
                , "undelete"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolStudents : String -> (Int, List (String)) -> Http.Request (List ((Student, (String, String))))
postSchoolStudents header_Authorization body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "students"
                ]
        , body =
            Http.jsonBody ((\(x, y) -> Json.Encode.list identity [ Json.Encode.int x, (Json.Encode.list  Json.Encode.string) y ]) body)
        , expect =
            Http.expectJson (list (map2 Tuple.pair (index 0 decodeStudent) (index 1 (map2 Tuple.pair (index 0 string) (index 1 string)))))
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolAnswers : String -> Maybe (Int) -> Maybe (String) -> Http.Request (List (Answer))
getSchoolAnswers header_Authorization query_story query_student =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_story
                    |> Maybe.map (String.fromInt >> Url.percentEncode >> (++) "story=")
                    |> Maybe.withDefault ""
                , query_student
                    |> Maybe.map (identity >> Url.percentEncode >> (++) "student=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                    ]
            , url =
                String.join "/"
                    [ "/api"
                    , "school"
                    , "answers"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson (list decodeAnswer)
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postSchoolAnswers : String -> Answer -> Http.Request (Answer)
postSchoolAnswers header_Authorization body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "answers"
                ]
        , body =
            Http.jsonBody (encodeAnswer body)
        , expect =
            Http.expectJson decodeAnswer
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolLeaderboard : String -> Http.Request (List (LeaderBoardEntry))
getSchoolLeaderboard header_Authorization =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "leaderboard"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeLeaderBoardEntry)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSchoolTeachers : String -> Http.Request (List ((Teacher, Bool)))
getSchoolTeachers header_Authorization =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "teachers"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list (map2 Tuple.pair (index 0 decodeTeacher) (index 1 bool)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postSchoolTeachersByTeacherIdActivate : String -> String -> Http.Request (String)
postSchoolTeachersByTeacherIdActivate header_Authorization capture_teacherId =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "school"
                , "teachers"
                , capture_teacherId |> Url.percentEncode
                , "activate"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAnthologies : String -> Http.Request (List (Anthology))
getAnthologies header_Authorization =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "anthologies"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeAnthology)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAnthologies : String -> Anthology -> Http.Request (Anthology)
postAnthologies header_Authorization body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "anthologies"
                ]
        , body =
            Http.jsonBody (encodeAnthology body)
        , expect =
            Http.expectJson decodeAnthology
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAnthologiesByAnthologyIdStarter_stories : String -> String -> Http.Request (NoContent)
postAnthologiesByAnthologyIdStarter_stories header_Authorization capture_anthologyId =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "anthologies"
                , capture_anthologyId |> Url.percentEncode
                , "starter_stories"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAnthologiesByAnthologyId : String -> String -> Anthology -> Http.Request (Anthology)
postAnthologiesByAnthologyId header_Authorization capture_anthologyId body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "anthologies"
                , capture_anthologyId |> Url.percentEncode
                ]
        , body =
            Http.jsonBody (encodeAnthology body)
        , expect =
            Http.expectJson decodeAnthology
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteAnthologiesByAnthologyId : String -> String -> Http.Request (String)
deleteAnthologiesByAnthologyId header_Authorization capture_anthologyId =
    Http.request
        { method =
            "DELETE"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "anthologies"
                , capture_anthologyId |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAuthenticate : LoginRequest -> Http.Request (Login)
postAuthenticate body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "/api"
                , "authenticate"
                ]
        , body =
            Http.jsonBody (encodeLoginRequest body)
        , expect =
            Http.expectJson decodeLogin
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAccountSettings : String -> Value -> Http.Request (NoContent)
postAccountSettings header_Authorization body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "account"
                , "settings"
                ]
        , body =
            Http.jsonBody (identity body)
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAccountRegister : String -> Registration -> Http.Request (NoContent)
postAccountRegister header_Authorization body =
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "account"
                , "register"
                ]
        , body =
            Http.jsonBody (encodeRegistration body)
        , expect =
            Http.expectStringResponse
                (\res ->
                    if String.isEmpty res.body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAccountRegisterCode : String -> Http.Request (String)
getAccountRegisterCode header_Authorization =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "account"
                , "register"
                , "code"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAdminStats : String -> Http.Request (Value)
getAdminStats header_Authorization =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization") (Just header_Authorization)
                ]
        , url =
            String.join "/"
                [ "/api"
                , "admin"
                , "stats"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson value
        , timeout =
            Nothing
        , withCredentials =
            False
        }

decodeStory : Decoder Story
decodeStory =
    Json.Decode.succeed Story
        |> required "id" int
        |> required "title" string
        |> required "img" string
        |> required "level" int
        |> required "qualification" (nullable string)
        |> required "curriculum" (nullable string)
        |> required "tags" (list string)
        |> required "content" string
        |> required "words" (list decodeDictEntry)
        |> required "clarifyWord" string
        |> required "enabled" bool
        |> required "createdAt" int

encodeStory : Story -> Json.Encode.Value
encodeStory x =
    Json.Encode.object
        [ ( "id", Json.Encode.int x.id )
        , ( "title", Json.Encode.string x.title )
        , ( "img", Json.Encode.string x.img )
        , ( "level", Json.Encode.int x.level )
        , ( "qualification", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.qualification )
        , ( "curriculum", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.curriculum )
        , ( "tags", (Json.Encode.list  Json.Encode.string) x.tags )
        , ( "content", Json.Encode.string x.content )
        , ( "words", (Json.Encode.list  encodeDictEntry) x.words )
        , ( "clarifyWord", Json.Encode.string x.clarifyWord )
        , ( "enabled", Json.Encode.bool x.enabled )
        , ( "createdAt", Json.Encode.int x.createdAt )
        ]

decodeDictEntry : Decoder DictEntry
decodeDictEntry =
    Json.Decode.succeed DictEntry
        |> required "word" string
        |> required "index" int

encodeDictEntry : DictEntry -> Json.Encode.Value
encodeDictEntry x =
    Json.Encode.object
        [ ( "word", Json.Encode.string x.word )
        , ( "index", Json.Encode.int x.index )
        ]

decodeSchool : Decoder School
decodeSchool =
    Json.Decode.succeed School
        |> required "id" string
        |> required "name" string
        |> required "description" (nullable string)

encodeSchool : School -> Json.Encode.Value
encodeSchool x =
    Json.Encode.object
        [ ( "id", Json.Encode.string x.id )
        , ( "name", Json.Encode.string x.name )
        , ( "description", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.description )
        ]

decodeAnswer : Decoder Answer
decodeAnswer =
    Json.Decode.succeed Answer
        |> required "storyId" int
        |> required "studentId" string
        |> required "connect" string
        |> required "question" string
        |> required "summarise" string
        |> required "clarify" string

encodeAnswer : Answer -> Json.Encode.Value
encodeAnswer x =
    Json.Encode.object
        [ ( "storyId", Json.Encode.int x.storyId )
        , ( "studentId", Json.Encode.string x.studentId )
        , ( "connect", Json.Encode.string x.connect )
        , ( "question", Json.Encode.string x.question )
        , ( "summarise", Json.Encode.string x.summarise )
        , ( "clarify", Json.Encode.string x.clarify )
        ]

decodeClass : Decoder Class
decodeClass =
    Json.Decode.succeed Class
        |> required "id" string
        |> required "name" string
        |> required "description" (nullable string)
        |> required "schoolId" string
        |> required "createdBy" string
        |> required "students" (list string)

encodeClass : Class -> Json.Encode.Value
encodeClass x =
    Json.Encode.object
        [ ( "id", Json.Encode.string x.id )
        , ( "name", Json.Encode.string x.name )
        , ( "description", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.description )
        , ( "schoolId", Json.Encode.string x.schoolId )
        , ( "createdBy", Json.Encode.string x.createdBy )
        , ( "students", (Json.Encode.list  Json.Encode.string) x.students )
        ]

decodeLogin : Decoder Login
decodeLogin =
    Json.Decode.succeed Login
        |> required "sub" string
        |> required "username" string
        |> required "name" string
        |> required "role" decodeUserType
        |> required "level" int
        |> required "settings" (nullable value)
        |> required "token" string

encodeLogin : Login -> Json.Encode.Value
encodeLogin x =
    Json.Encode.object
        [ ( "sub", Json.Encode.string x.sub )
        , ( "username", Json.Encode.string x.username )
        , ( "name", Json.Encode.string x.name )
        , ( "role", encodeUserType x.role )
        , ( "level", Json.Encode.int x.level )
        , ( "settings", (Maybe.withDefault Json.Encode.null << Maybe.map identity) x.settings )
        , ( "token", Json.Encode.string x.token )
        ]

decodeUserType : Decoder UserType
decodeUserType =
    Json.Decode.succeed UserType
        |> required "userType" string

encodeUserType : UserType -> Json.Encode.Value
encodeUserType x =
    Json.Encode.object
        [ ( "userType", Json.Encode.string x.userType )
        ]

decodeStudent : Decoder Student
decodeStudent =
    Json.Decode.succeed Student
        |> required "id" string
        |> required "name" string
        |> required "description" (nullable string)
        |> required "level" int
        |> required "schoolId" string
        |> required "hidden" bool
        |> required "deleted" (nullable float)

encodeStudent : Student -> Json.Encode.Value
encodeStudent x =
    Json.Encode.object
        [ ( "id", Json.Encode.string x.id )
        , ( "name", Json.Encode.string x.name )
        , ( "description", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.description )
        , ( "level", Json.Encode.int x.level )
        , ( "schoolId", Json.Encode.string x.schoolId )
        , ( "hidden", Json.Encode.bool x.hidden )
        , ( "deleted", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.float) x.deleted )
        ]

decodeTeacher : Decoder Teacher
decodeTeacher =
    Json.Decode.succeed Teacher
        |> required "id" string
        |> required "name" string
        |> required "bio" (nullable string)
        |> required "schoolId" string

encodeTeacher : Teacher -> Json.Encode.Value
encodeTeacher x =
    Json.Encode.object
        [ ( "id", Json.Encode.string x.id )
        , ( "name", Json.Encode.string x.name )
        , ( "bio", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.bio )
        , ( "schoolId", Json.Encode.string x.schoolId )
        ]

decodeAnthology : Decoder Anthology
decodeAnthology =
    Json.Decode.succeed Anthology
        |> required "id" string
        |> required "name" string
        |> required "description" string
        |> required "createdBy" string
        |> required "schoolId" (nullable string)
        |> required "stories" (list int)
        |> required "hidden" bool

encodeAnthology : Anthology -> Json.Encode.Value
encodeAnthology x =
    Json.Encode.object
        [ ( "id", Json.Encode.string x.id )
        , ( "name", Json.Encode.string x.name )
        , ( "description", Json.Encode.string x.description )
        , ( "createdBy", Json.Encode.string x.createdBy )
        , ( "schoolId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.schoolId )
        , ( "stories", (Json.Encode.list  Json.Encode.int) x.stories )
        , ( "hidden", Json.Encode.bool x.hidden )
        ]

decodeLoginRequest : Decoder LoginRequest
decodeLoginRequest =
    Json.Decode.succeed LoginRequest
        |> required "username" string
        |> required "password" string
        |> required "otp" (nullable int)
        |> required "ua" string

encodeLoginRequest : LoginRequest -> Json.Encode.Value
encodeLoginRequest x =
    Json.Encode.object
        [ ( "username", Json.Encode.string x.username )
        , ( "password", Json.Encode.string x.password )
        , ( "otp", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.otp )
        , ( "ua", Json.Encode.string x.ua )
        ]

decodeLeaderBoardEntry : Decoder LeaderBoardEntry
decodeLeaderBoardEntry =
    Json.Decode.succeed LeaderBoardEntry
        |> required "position" int
        |> required "name" string
        |> required "studentId" string
        |> required "score" int

encodeLeaderBoardEntry : LeaderBoardEntry -> Json.Encode.Value
encodeLeaderBoardEntry x =
    Json.Encode.object
        [ ( "position", Json.Encode.int x.position )
        , ( "name", Json.Encode.string x.name )
        , ( "studentId", Json.Encode.string x.studentId )
        , ( "score", Json.Encode.int x.score )
        ]

decodeRegistration : Decoder Registration
decodeRegistration =
    Json.Decode.succeed Registration
        |> required "email" string
        |> required "code" (nullable string)
        |> required "schoolName" string
        |> required "teacherName" string
        |> required "password" string

encodeRegistration : Registration -> Json.Encode.Value
encodeRegistration x =
    Json.Encode.object
        [ ( "email", Json.Encode.string x.email )
        , ( "code", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.code )
        , ( "schoolName", Json.Encode.string x.schoolName )
        , ( "teacherName", Json.Encode.string x.teacherName )
        , ( "password", Json.Encode.string x.password )
        ]