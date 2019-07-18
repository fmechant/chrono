module Chrono.TimeTests exposing (all)

import Chrono.Moment as Moment
import Chrono.TestUtils exposing (..)
import Chrono.Time exposing (..)
import Chrono.TimeZone as TimeZone
import Expect
import Fuzz
import Test exposing (..)
import Time


all : Test
all =
    describe "fromMoment should use correct conversion to time of day."
        [ test "Epoch moment is -43,200,000 in utc." <|
            \() ->
                Moment.fromMsSinceEpoch 0
                    |> fromMoment TimeZone.utc
                    |> toMsSinceNoon
                    |> Expect.equal -43200000
        , test "construct time using AM" <|
            \() ->
                am 7
                    |> m 15
                    |> toMsSinceNoon
                    |> Expect.equal (-5 * 3600000 + 15 * 60000)
        , test "construct time using PM" <|
            \() ->
                pm 7
                    |> m 15
                    |> toMsSinceNoon
                    |> Expect.equal (7 * 3600000 + 15 * 60000)
        , test "12 AM is midnight" <|
            \() ->
                am 12
                    |> m 0
                    |> Expect.equal midnight
        , test "12 PM is noon" <|
            \() ->
                pm 12
                    |> m 0
                    |> Expect.equal noon
        , fuzz (Fuzz.intRange 1 11) "am and h24 are interchangeable" <|
            \hour ->
                am hour
                    |> Expect.equal (h24 hour)
        , fuzz (Fuzz.intRange 1 11) "pm and h24 are interchangeable" <|
            \hour ->
                pm hour
                    |> Expect.equal (h24 (12 + hour))
        ]
