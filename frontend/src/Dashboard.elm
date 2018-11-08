module Dashboard exposing (Stats, decodeStats, view)

import Axis
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (style)
import Json.Decode as Json exposing (Decoder, float, int, list, string, nullable)
import Json.Decode.Pipeline exposing (optional, required)
import List.Extra as List
import Path exposing (Path)
import Scale exposing (BandScale, ContinuousScale, OrdinalScale, Scale, defaultBandConfig)
import Scale.Color
import Shape exposing (StackConfig, StackResult)
import Time exposing (Month(..))
import Time.Extra as Time exposing (Parts, partsToPosix, posixToParts)
import Tuple exposing (pair)
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (class, fill, fontFamily, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


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
    { storyActivityDaily : List ( String, List Float )
    , storyActivityMonthly : List ( String, List Float )
    , schools : List School
    , sampleTime : Time.Posix
    }

epochTimeDecoder : Decoder Time.Posix
epochTimeDecoder =
    (Json.map ((*) 1000 >> Time.millisToPosix) int)

decodeStats : Decoder Stats
decodeStats =
    Json.succeed Stats
        |> required "story_activity_daily" (list decodeSchoolActivity)
        |> required "story_activity_monthly" (list decodeSchoolActivity)
        |> required "schools" (list decodeSchoolData)
        |> required "sample_time" (Json.map ((*) 1000 >> Time.millisToPosix) int)


decodeSchoolActivity : Decoder ( String, List Float )
decodeSchoolActivity =
    Json.succeed pair
        |> required "school_name" string
        |> required "story_activity" (list float)


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


fromCalendarDate : Int -> Month -> Int -> Time.Posix
fromCalendarDate year month day =
    Time.partsToPosix Time.utc (Parts year month day 0 0 0 0)


view : Stats -> { title : String, content : Html msg }
view { storyActivityDaily, storyActivityMonthly, sampleTime, schools } =
    let
        now =
            posixToParts Time.utc sampleTime

        endMonth =
            Time.ceiling Time.Month Time.utc sampleTime

        startMonth =
            Time.add Time.Month -25 Time.utc endMonth

        config =
            { data = storyActivityMonthly
            , offset = Shape.stackOffsetNone
            , order = List.sortBy (Tuple.second >> List.sum >> negate)
            }
    in
        { title = "Admin Dashboard"
        , content =
              div [ style "max-width" "1000px", style "margin" "auto" ]
                  [ viewStackedBars (Shape.stack { config | data = storyActivityDaily })
                  , viewStack ( startMonth, endMonth ) (Shape.stack { config | data = storyActivityMonthly })
                  , viewSchools schools
                  ]
       }


viewSchools : List School -> Html msg
viewSchools schools =
    let
        viewSchool s =
            div []
                [ h1 [] [ text s.name ]
                , p [] [ text (String.fromInt s.numberOfStudents ++ " students.") ]
                , table [ ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Name" ]
                            , th [] [ text "Email" ]
                            ]
                        ]
                    , tbody [] (List.map teacherRow s.teachers)
                    ]
                ]

        teacherRow t =
            tr []
                [ td [] [ text t.name ]
                , td [] [ text t.email ]
                ]
    in
    div [] ( List.map viewSchool schools )


viewStack : ( Time.Posix, Time.Posix ) -> StackResult String -> Html msg
viewStack interval { values, labels, extent } =
    let
        labelsWidth =
            50

        padding : Float
        padding =
            40

        size : Int
        size =
            List.head values
                |> Maybe.map List.length
                |> Maybe.withDefault 0

        xScale : ContinuousScale Float
        xScale =
            -- map an index to screen space
            Scale.linear ( padding, w - padding - labelsWidth ) ( 0, toFloat size )

        yScale : ContinuousScale Float
        yScale =
            Scale.linear ( h - padding, padding ) extent
                |> Scale.nice 5

        xAxis : Svg msg
        xAxis =
            Scale.time Time.utc ( 0, w - padding * 2 - labelsWidth ) interval
                |> Axis.bottom []

        yAxis : Svg msg
        yAxis =
            Axis.left [ Axis.tickFormat (String.fromInt << round), Axis.tickCount 5 ] yScale

        paths =
            List.map2 (renderStream ( xScale, yScale )) colors values

        labelPositions =
            let
                position ys =
                    ys
                        |> List.last
                        |> Maybe.withDefault ( 0, 0 )
                        |> (\( y1, y2 ) -> (y2 + y1) / 2)
                        |> Scale.convert yScale
            in
            List.indexedMap (\i _ -> 100 + toFloat i * 10) labels

        labelElement : String -> Float -> Svg msg
        labelElement label yPosition =
            g [ transform [ Translate (w - padding - labelsWidth + 10) yPosition ] ]
                [ text_ [ fill (sampleColor label |> Fill) ] [ text label ] ]

        colorScale : OrdinalScale String Color
        colorScale =
            Scale.ordinal Scale.Color.category10 (List.reverse labels)

        sampleColor : String -> Color
        sampleColor label =
            Scale.convert colorScale label |> Maybe.withDefault Color.black

        colors : List Color
        colors =
            List.map sampleColor labels
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis ]
        , g [ transform [ Translate padding 0 ] ]
            [ yAxis
            , text_ [ fontFamily [ "sans-serif" ], fontSize 14, x 5, y 65 ] [ text "Stories" ]
            ]
        , g [ class [ "series" ] ] paths
        , g [ fontFamily [ "sans-serif" ], fontSize 10 ]
            (List.map2 labelElement labels labelPositions)
        ]


{-| Renders one colored stream with given scaling
-}
renderStream : ( ContinuousScale Float, ContinuousScale Float ) -> Color -> List ( Float, Float ) -> Svg msg
renderStream scales color coords =
    Path.element (toArea scales coords) [ fill (Fill color) ]


{-| Create a svg path string that draws the area between two lines
-}
toArea : ( ContinuousScale Float, ContinuousScale Float ) -> List ( Float, Float ) -> Path
toArea ( scaleX, scaleY ) ys =
    let
        mapper : Int -> ( Float, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        mapper index ( y1, y2 ) =
            let
                xCoord =
                    index
                        |> toFloat
                        |> Scale.convert scaleX

                ( low, high ) =
                    if y1 < y2 then
                        ( y1, y2 )

                    else
                        ( y2, y1 )
            in
            Just
                ( ( xCoord, Scale.convert scaleY low )
                , ( xCoord, Scale.convert scaleY high )
                )
    in
    List.indexedMap mapper ys
        |> Shape.area Shape.monotoneInXCurve


viewStackedBars : StackResult String -> Svg msg
viewStackedBars { values, labels, extent } =
    let
        nDays : Int
        nDays =
            List.head values
                |> Maybe.map List.length
                |> Maybe.withDefault 0

        nBars =
            List.length labels

        days =
            List.range 1 nDays

        -- Switch from values per school to per day
        dayValues =
            List.transpose values

        xScale : BandScale Int
        xScale =
            Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - (padding.top + padding.bottom) ) days

        padding : { bottom : Float, left : Float, right : Float, top : Float }
        padding =
            { top = 30
            , left = 60
            , right = 30
            , bottom = 60
            }

        yScale : ContinuousScale Float
        yScale =
            Scale.linear ( h - (padding.left + padding.right), 0 ) extent
                |> Scale.nice 4

        scaledValues : List (List ( Float, Float ))
        scaledValues =
            List.map (List.map (\( y1, y2 ) -> ( Scale.convert yScale y1, Scale.convert yScale y2 ))) dayValues

        colors : List Color
        colors =
            let
                reverseViridis progression =
                    Scale.Color.viridisInterpolator (1 - progression)

                colorScale =
                    Scale.sequential reverseViridis ( 0, toFloat nBars - 1 )
                        |> Scale.convert
            in
            List.map (colorScale << toFloat) (List.range 0 (nBars - 1))

        column : ( Int, List ( Float, Float ) ) -> Svg msg
        column ( day, values_ ) =
            let
                block color ( upperY, lowerY ) =
                    rect
                        [ x <| Scale.convert xScale day
                        , y <| lowerY
                        , width <| Scale.bandwidth xScale
                        , height <| (abs <| upperY - lowerY)
                        , fill (Fill color)
                        ]
                        []
            in
            g [ class [ "column" ] ] (List.map2 block colors values_)
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding.left - 1) (h - padding.bottom) ] ]
            [ Axis.bottom [ Axis.tickCount 10 ] (Scale.toRenderable String.fromInt xScale) ]
        , g [ transform [ Translate (padding.left - 1) padding.top ] ]
            [ Axis.left [] yScale ]
        , g [ transform [ Translate padding.left padding.top ], class [ "series" ] ] <|
            List.map column (List.map2 (\a b -> ( a, b )) days scaledValues)
        ]
