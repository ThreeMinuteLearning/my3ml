port module Dashboard exposing (Stats, decodeStats, elmToVega, vegaSpec, view)

import Axis
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Json.Decode as Json exposing (Decoder, float, int, list, nullable, string, value)
import Json.Decode.Pipeline exposing (optional, required)
import List.Extra as List
import Time exposing (Month(..))
import Tuple exposing (pair)
import VegaLite exposing (..)


type alias Teacher =
    { name : String
    , email : String
    , createdAt : Time.Posix
    , lastLogin : Maybe Time.Posix
    }


type alias School =
    { name : String
    , teachers : List Teacher
    , numberOfStudents : Int
    , createdAt : Time.Posix
    }


type alias Stats =
    { storyActivityDaily : Json.Value
    , storyActivityMonthly : Json.Value
    , schools : List School
    , sampleTime : Time.Posix
    }


port elmToVega : Spec -> Cmd msg


vegaSpec : Stats -> Spec
vegaSpec stats =
    toVegaLite
        [ hConcat
            [ storyActivityDailySpec stats
            , storyActivityMonthlySpec stats
            ]
        ]


storyActivityDailySpec : Stats -> Spec
storyActivityDailySpec stats =
    let
        data =
            dataFromJson stats.storyActivityDaily []

        enc =
            encoding
                << position X [ pName "date", pMType Temporal, pTimeUnit monthDate ]
                << position Y [ pName "stories", pMType Quantitative, pAggregate opSum ]
                << color [ mName "school_name", mMType Nominal ]
    in
    toVegaLite [ data, width 400, bar [], enc [] ]


storyActivityMonthlySpec : Stats -> Spec
storyActivityMonthlySpec stats =
    let
        data =
            dataFromJson stats.storyActivityMonthly []

        enc =
            encoding
                << position X [ pName "date", pMType Temporal, pTimeUnit yearMonth ]
                << position Y [ pName "stories", pMType Quantitative, pAggregate opSum ]
                << color [ mName "school_name", mMType Nominal ]
    in
    toVegaLite [ data, width 400, area [], enc [] ]


epochTimeDecoder : Decoder Time.Posix
epochTimeDecoder =
    Json.map ((*) 1000 >> Time.millisToPosix) int


decodeStats : Decoder Stats
decodeStats =
    Json.succeed Stats
        |> required "story_activity_daily" value
        |> required "story_activity_monthly" value
        |> required "schools" (Json.map (List.sortBy (\s -> -1 * s.numberOfStudents)) (list decodeSchoolData))
        |> required "sample_time" (Json.map ((*) 1000 >> Time.millisToPosix) int)


decodeSchoolData : Decoder School
decodeSchoolData =
    Json.succeed School
        |> required "name" string
        |> required "teachers" (list decodeTeacher)
        |> required "number_of_students" int
        |> required "created_at" epochTimeDecoder


decodeTeacher : Decoder Teacher
decodeTeacher =
    Json.succeed Teacher
        |> required "name" string
        |> required "email" string
        |> required "created_at" epochTimeDecoder
        |> required "last_login" (nullable epochTimeDecoder)


w : Float
w =
    990


h : Float
h =
    504


view : Stats -> { title : String, content : Html msg }
view { storyActivityDaily, storyActivityMonthly, sampleTime, schools } =
    { title = "Admin Dashboard"
    , content =
        div [ class "container mx-auto" ]
            [ div [ id "vis" ] []
            , viewSchools schools
            ]
    }


viewSchools : List School -> Html msg
viewSchools schools =
    let
        viewSchool s =
            li [ class "flex flex-col px-3 mb-6 w-full md:w-1/2 lg:w-1/3" ]
                [ div [ class "flex flex-col bg-white rounded shadow-md flex-1 overflow-x-auto" ]
                    [ div [ class "px-6 py-4" ]
                        [ h1 [ class "text-xl font-bold" ] [ Html.text s.name ]
                        , p [ class "text-gray-darker text-base" ] [ Html.text (String.fromInt s.numberOfStudents ++ " students.") ]
                        , div [ class "flex flex-col px-1 py-4" ]
                            (List.map viewTeacher s.teachers)
                        ]
                    ]
                ]

        viewTeacher t =
            div [ class "flex flex-col py-2" ]
                [ div [ class "px-1 text-l font-semibold" ] [ Html.text t.name ]
                , div [ class "px-1 text-l" ] [ Html.text t.email ]
                ]
    in
    ul [ class "list-reset flex flex-row w-full py-16 flex-wrap md:m-0 lg:-mx-3" ] (List.map viewSchool schools)
