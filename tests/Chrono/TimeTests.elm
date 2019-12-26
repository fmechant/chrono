module Chrono.TimeTests exposing (all)

import Chrono.Moment as Moment
import Chrono.TestUtils exposing (..)
import Chrono.Time as Time exposing (..)
import Chrono.TimeZone as TimeZone
import Expect
import Fuzz
import Test exposing (..)
import Time


all : Test
all =
    describe "time"
        [ describe "fromMoment should use correct conversion to time of day."
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
        , describe "view"
            [ test "view time for noon" <|
                \() ->
                    noon
                        |> Time.view
                        |> Expect.equal
                            { hour24 = 12
                            , minute = 0
                            , second = 0
                            , millisecond = 0
                            }
            , test "to12Hours for noon" <|
                \() ->
                    noon
                        |> Time.view
                        |> .hour24
                        |> to12Hours
                        |> Expect.equal ( 12, PM )
            , test "to12Hours for mightnight" <|
                \() ->
                    midnight
                        |> Time.view
                        |> .hour24
                        |> to12Hours
                        |> Expect.equal ( 12, AM )
            ]
        , describe "milliseconds"
            [ test "milliseconds get overridden" <|
                \() ->
                    (h24 13 |> m 5 |> ms 100)
                        |> ms 45
                        |> Expect.equal (h24 13 |> m 5 |> ms 45)
            , test "milliseconds get overridden for AM too" <|
                \() ->
                    (h24 2 |> m 5 |> ms 100)
                        |> ms 45
                        |> Expect.equal (h24 2 |> m 5 |> ms 45)
            ]
        , daylightSavings
        ]


daylightSavings : Test
daylightSavings =
    let
        switchMoment =
            -- 2019-03-21 03:00:00 GMT+02:00 DST
            Moment.fromMsSinceEpoch 1553994000000

        zone =
            daylightSavingsTimeZone switchMoment
    in
    describe "handle daylight-savings in fromMoment"
        [ test "before switch should return just before 2" <|
            \() ->
                Moment.intoPast (Moment.minutes 10) switchMoment
                    |> Time.fromMoment zone
                    |> Expect.equal (h24 1 |> m 50)
        , test "after switch should return just after 3" <|
            \() ->
                Moment.intoFuture (Moment.minutes 10) switchMoment
                    |> Time.fromMoment zone
                    |> Expect.equal (h24 3 |> m 10)
        ]
