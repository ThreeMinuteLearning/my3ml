port module Dashboard exposing (SortSchools(..), Stats, decodeStats, elmToVega, sortSchools, vegaSpec, view)

import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, id, selected, style)
import Html.Events exposing (on, targetValue)
import Json.Decode as Json exposing (Decoder, bool, float, int, list, nullable, string, value)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra as List
import Time exposing (posixToMillis)
import Tuple exposing (pair)
import Util exposing (posixToString)
import VegaLite exposing (..)
import Views.Form as Form


type alias Teacher =
    { name : String
    , email : String
    , admin : Bool
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
    , storyPopularityLastMonth : Json.Value
    , storyPopularityAllTime : Json.Value
    , schools : List School
    , sampleTime : Time.Posix
    , schoolsSortedBy : SortSchools
    }


type SortSchools
    = ByStudentCount
    | ByName
    | ByLastLogin
    | ByRegistrationDate


port elmToVega : Spec -> Cmd msg


vegaSpec : Stats -> Spec
vegaSpec stats =
    toVegaLite
        [ vConcat
            [ asSpec [ hConcat [ storyActivityDailySpec stats, storyActivityMonthlySpec stats ] ]
            , asSpec [ hConcat [ storyPopularitySpec "Popular stories this month" stats.storyPopularityLastMonth, storyPopularitySpec "Popular stories (all time)" stats.storyPopularityAllTime ] ]
            ]
        ]


storyActivityDailySpec : Stats -> Spec
storyActivityDailySpec stats =
    toVegaLite [ title "Story activity (daily)", dataFromJson stats.storyActivityDaily [], width 400, bar [], storyActivityEncoding monthDate [] ]


storyActivityMonthlySpec : Stats -> Spec
storyActivityMonthlySpec stats =
    toVegaLite [ title "Story activity (monthly)", dataFromJson stats.storyActivityMonthly [], width 400, area [], storyActivityEncoding yearMonth [] ]


storyActivityEncoding : TimeUnit -> List LabelledSpec -> ( VLProperty, Spec )
storyActivityEncoding timeUnit =
    encoding
        << position X [ pName "date", pMType Temporal, pTimeUnit timeUnit ]
        << position Y [ pName "stories", pMType Quantitative, pTitle "Stories completed", pAggregate opSum ]
        << color [ mName "school_name", mTitle "School name", mMType Nominal ]


storyPopularitySpec : String -> Json.Value -> Spec
storyPopularitySpec title_ jsonData =
    let
        enc =
            encoding
                << position X [ pName "students", pMType Quantitative, pAggregate opSum, pTitle "Completed" ]
                << position Y [ pName "story_title", pTitle "Title", pMType Nominal, pSort [ soByField "students" opSum, soDescending ] ]
                << color [ mName "school_name", mMType Nominal ]
    in
    toVegaLite [ title title_, dataFromJson jsonData [], width 400, bar [], enc [] ]


epochTimeDecoder : Decoder Time.Posix
epochTimeDecoder =
    Json.map ((*) 1000 >> Time.millisToPosix) int


decodeStats : Decoder Stats
decodeStats =
    Json.succeed Stats
        |> required "story_activity_daily" value
        |> required "story_activity_monthly" value
        |> required "story_popularity_month" value
        |> required "story_popularity" value
        |> required "schools" (Json.map (List.sortBy (\s -> -1 * s.numberOfStudents)) (list decodeSchoolData))
        |> required "sample_time" (Json.map ((*) 1000 >> Time.millisToPosix) int)
        |> hardcoded ByStudentCount


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
        |> required "admin" bool
        |> required "created_at" epochTimeDecoder
        |> required "last_login" (nullable epochTimeDecoder)


sortSchools : SortSchools -> Stats -> Stats
sortSchools sortBy stats =
    let
        sorted =
            case sortBy of
                ByStudentCount ->
                    List.sortBy (\s -> -1 * s.numberOfStudents) stats.schools

                ByName ->
                    List.sortBy .name stats.schools

                ByLastLogin ->
                    List.sortBy (\s -> -1 * lastTeacherLogin s) stats.schools

                ByRegistrationDate ->
                    List.sortBy ((*) -1 << posixToMillis << .createdAt) stats.schools
    in
    { stats | schools = sorted, schoolsSortedBy = sortBy }


lastTeacherLogin : School -> Int
lastTeacherLogin s =
    List.maximum (List.filterMap (\t -> Maybe.map posixToMillis t.lastLogin) s.teachers)
        |> Maybe.withDefault 0


view : (SortSchools -> msg) -> Stats -> { title : String, content : Html msg }
view toSortMsg { storyActivityDaily, storyActivityMonthly, sampleTime, schools, schoolsSortedBy } =
    { title = "Admin Dashboard"
    , content =
        div [ class "container mx-auto" ]
            [ div [ id "vis" ] []
            , div [ class "flex mt-3" ]
                [ label [ class "mr-2" ] [ Html.text "Sort schools by" ]
                , viewSortSelect schoolsSortedBy toSortMsg
                ]
            , viewSchools schools
            ]
    }


viewSchools : List School -> Html msg
viewSchools schools =
    let
        lastLogin s =
            case lastTeacherLogin s of
                0 ->
                    "Never logged in."

                t ->
                    "Last teacher login: " ++ posixToString (Time.millisToPosix t)

        viewSchool s =
            li [ class "flex flex-col px-3 mb-6 w-full md:w-1/2 lg:w-1/3" ]
                [ div [ class "flex flex-col bg-white rounded shadow-md flex-1 overflow-x-auto" ]
                    [ div [ class "px-6 py-4" ]
                        [ h1 [ class "text-xl font-bold" ] [ Html.text s.name ]
                        , p [ class "text-gray-800 text-base" ]
                            [ Html.text (String.fromInt s.numberOfStudents ++ " students.")
                            ]
                        , p [ class "text-gray-800 text-sm" ]
                            [ Html.text ("Registered: " ++ posixToString s.createdAt)
                            ]
                        , p [ class "text-gray-800 text-sm" ]
                            [ Html.text (lastLogin s)
                            ]
                        , div [ class "flex flex-col px-1 py-3" ]
                            (List.map viewTeacher s.teachers)
                        ]
                    ]
                ]

        viewTeacher t =
            div [ class "flex flex-col py-2" ]
                [ div [ class "px-1 text-l font-semibold" ]
                    [ Html.text (t.name ++ if t.admin then " (admin)" else "")
                    ]
                , div [ class "px-1 text-l" ] [ Html.text t.email ]
                ]
    in
    ul [ class "flex flex-row w-full py-10 flex-wrap md:m-0 lg:-mx-3" ] (List.map viewSchool schools)


viewSortSelect : SortSchools -> (SortSchools -> msg) -> Html msg
viewSortSelect currentSelection mkMsg =
    let
        onSelect value =
            mkMsg <|
                case value of
                    "StudentCount" ->
                        ByStudentCount

                    "LastLogin" ->
                        ByLastLogin

                    "Name" ->
                        ByName

                    "RegistrationDate" ->
                        ByRegistrationDate

                    _ ->
                        ByStudentCount
    in
    Form.select [ on "change" (Json.map onSelect targetValue) ]
        [ Html.option
            [ Html.Attributes.selected (currentSelection == ByStudentCount)
            , Html.Attributes.value "StudentCount"
            ]
            [ Html.text "Number of students" ]
        , Html.option
            [ Html.Attributes.selected (currentSelection == ByName)
            , Html.Attributes.value "Name"
            ]
            [ Html.text "School name" ]
        , Html.option
            [ Html.Attributes.selected (currentSelection == ByLastLogin)
            , Html.Attributes.value "LastLogin"
            ]
            [ Html.text "Last teacher login" ]
        , Html.option
            [ Html.Attributes.selected (currentSelection == ByRegistrationDate)
            , Html.Attributes.value "RegistrationDate"
            ]
            [ Html.text "Registration date" ]
        ]
