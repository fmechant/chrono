module Chrono.MomentTests exposing (all)

import Chrono.Moment exposing (..)
import Chrono.TestUtils exposing (..)
import Expect
import Fuzz
import Test exposing (..)
import Time


all : Test
all =
    describe "Moments and Durations"
        [ describe "Moments and milliseconds since epoch should be interchangeable."
            [ fuzz Fuzz.int "A moment from milliseconds since epoch should be the same number of milliseconds since epoch." <|
                \msSinceEpoch ->
                    msSinceEpoch
                        |> fromMsSinceEpoch
                        |> toMsAfterEpoch
                        |> Expect.equal msSinceEpoch
            ]
        , describe "Durations' arithmetic should be correct."
            [ test "combining milliseconds and seconds" <|
                \() ->
                    milliseconds 23
                        |> and seconds 5
                        |> durationView
                        |> Expect.equal { milliseconds = 23, seconds = 5, minutes = 0, hours = 0 }
            , test "combining overlaping milliseconds and seconds" <|
                \() ->
                    milliseconds 1015
                        |> and seconds 5
                        |> durationView
                        |> Expect.equal { milliseconds = 15, seconds = 6, minutes = 0, hours = 0 }
            , test "combining overlaping milliseconds, seconds, minutes and hours" <|
                \() ->
                    milliseconds 100015
                        |> and seconds 75
                        |> and minutes 190
                        |> and hours 20
                        |> durationView
                        |> Expect.equal { milliseconds = 15, seconds = 55, minutes = 12, hours = 23 }
            , fuzz2 fuzzMoment fuzzDuration "elapsed should return a duration that gives the moment if we move it into the future" <|
                \moment duration ->
                    moment
                        |> intoFuture duration
                        |> elapsed moment
                        |> Expect.equal ( duration, IntoTheFuture )
            , fuzz2 fuzzMoment fuzzNonZeroDuration "elapsed should return a duration that gives the moment if we move it into the past" <|
                \moment duration ->
                    moment
                        |> intoPast duration
                        |> elapsed moment
                        |> Expect.equal ( duration, IntoThePast )
            ]
        , describe "Moving Moments with Durations."
            [ fuzz2 fuzzMoment fuzzDuration "Moving into future and back into past should return to same moment" <|
                \moment duration ->
                    moment
                        |> intoFuture duration
                        |> intoPast duration
                        |> Expect.equal moment
            , test "intoFuture moves into future." <|
                \() ->
                    fromMsSinceEpoch 500
                        |> intoFuture
                            (hours 20
                                |> and minutes 190
                                |> and seconds 75
                                |> and milliseconds 100015
                            )
                        |> toMsAfterEpoch
                        |> Expect.equal (500 + 100015 + 75 * 1000 + 190 * 60000 + 20 * 3600000)
            , test "intoPast moves into past." <|
                \() ->
                    fromMsSinceEpoch 500
                        |> intoPast
                            (hours 20
                                |> and minutes 190
                                |> and seconds 75
                                |> and milliseconds 100015
                            )
                        |> toMsAfterEpoch
                        |> Expect.equal (500 - 100015 - 75 * 1000 - 190 * 60000 - 20 * 3600000)
            ]
        ]
