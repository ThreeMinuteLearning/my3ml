module Zxcvbn exposing (Zxcvbn, ZxcvbnFeedback, decodeZxcvbn)

import Json.Decode exposing (field)
import Json.Decode.Pipeline exposing (required)


type alias Zxcvbn =
    { password :
        String
        -- , guesses : Float
        -- , guessesLog10 : Float
    , calcTime :
        Int
        -- , crackTimesSeconds : CrackTimesSeconds
        -- , crackTimesDisplay : CrackTimesDisplay
    , score : Int
    , feedback : ZxcvbnFeedback
    }


type alias CrackTimesSeconds =
    { onlineThrottling100PerHour : Float
    , onlineNoThrottling10PerSecond : Float
    , offlineSlowHashing1e4PerSecond : Float
    , offlineFastHashing1e10PerSecond : Float
    }


type alias CrackTimesDisplay =
    { onlineThrottling100PerHour : String
    , onlineNoThrottling10PerSecond : String
    , offlineSlowHashing1e4PerSecond : String
    , offlineFastHashing1e10PerSecond : String
    }


type alias ZxcvbnFeedback =
    { warning : String
    , suggestions : List String
    }


decodeZxcvbn : Json.Decode.Decoder Zxcvbn
decodeZxcvbn =
    Json.Decode.succeed Zxcvbn
        |> required "password" (Json.Decode.string)
        -- |> required "guesses" (Json.Decode.float)
        -- |> required "guesses_log10" (Json.Decode.float)
        |>
            required "calc_time" (Json.Decode.int)
        -- |> required "crack_times_seconds" (decodeCrackTimesSeconds)
        -- |> required "crack_times_display" (decodeCrackTimesDisplay)
        |>
            required "score" (Json.Decode.int)
        |> required "feedback" (decodeZxcvbnFeedback)


decodeCrackTimesSeconds : Json.Decode.Decoder CrackTimesSeconds
decodeCrackTimesSeconds =
    Json.Decode.map4 CrackTimesSeconds
        (field "online_throttling_100_per_hour" Json.Decode.float)
        (field "online_no_throttling_10_per_second" Json.Decode.float)
        (field "offline_slow_hashing_1e4_per_second" Json.Decode.float)
        (field "offline_fast_hashing_1e10_per_second" Json.Decode.float)


decodeCrackTimesDisplay : Json.Decode.Decoder CrackTimesDisplay
decodeCrackTimesDisplay =
    Json.Decode.map4 CrackTimesDisplay
        (field "online_throttling_100_per_hour" Json.Decode.string)
        (field "online_no_throttling_10_per_second" Json.Decode.string)
        (field "offline_slow_hashing_1e4_per_second" Json.Decode.string)
        (field "offline_fast_hashing_1e10_per_second" Json.Decode.string)


decodeZxcvbnFeedback : Json.Decode.Decoder ZxcvbnFeedback
decodeZxcvbnFeedback =
    Json.Decode.map2 ZxcvbnFeedback
        (field "warning" Json.Decode.string)
        (field "suggestions" (Json.Decode.list Json.Decode.string))
