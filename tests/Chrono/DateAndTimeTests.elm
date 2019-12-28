module Chrono.DateAndTimeTests exposing (all)

import Chrono.Date as Date
import Chrono.DateAndTime exposing (..)
import Chrono.Moment as Moment
import Chrono.TestUtils exposing (..)
import Chrono.Time as Time exposing (h24, m)
import Expect
import Fuzz
import Test exposing (..)


all : Test
all =
    describe "Date and Time tests"
        [ describe "fromMoment"
            [ test "should give the same date and time as the specific fromMoments" <|
                \() ->
                    let
                        moment =
                            Moment.fromMsSinceEpoch 120000

                        timeZone =
                            Moment.utc
                    in
                    moment
                        |> fromMoment timeZone
                        |> Expect.equal { date = Date.fromMoment timeZone moment, time = Time.fromMoment timeZone moment }
            , fuzz fuzzMoment "converting the date and time and back to moment should be consistent." <|
                \aMoment ->
                    aMoment
                        |> fromMoment Moment.utc
                        |> toMoment Moment.utc
                        |> Expect.equal aMoment
            ]
        , daylightSavingsSpecific
        ]


daylightSavingsSpecific : Test
daylightSavingsSpecific =
    let
        switchMoment =
            -- 2019-03-21 03:00:00 GMT+02:00 DST
            Moment.fromMsSinceEpoch 1553994000000

        zone =
            Moment.customZone 60 [ { start = 25899900, offset = 120 } ]

        date =
            Date.fromMoment zone switchMoment

        oneAClock =
            h24 1 |> m 0

        fourAClock =
            h24 4 |> m 0
    in
    describe "toMoment"
        [ test "should give correct moment before switch" <|
            \() ->
                date
                    |> withTime oneAClock
                    |> toMoment zone
                    |> Expect.equal (Moment.intoPast (Moment.hours 1) switchMoment)
        , test "should give correct moment after switch" <|
            \() ->
                date
                    |> withTime fourAClock
                    |> toMoment zone
                    |> Expect.equal (Moment.intoFuture (Moment.hours 1) switchMoment)
        ]
