module Chrono.MomentTests exposing (all)

import Chrono.Moment exposing (..)
import Chrono.TestUtils exposing (..)
import Expect
import Fuzz
import Test exposing (..)
import Time


all : Test
all =
    describe "Moments, TimeLapses"
        [ describe "Moments and milliseconds since epoch should be interchangeable."
            [ fuzz Fuzz.int "A moment from milliseconds since epoch should be the same number of milliseconds since epoch." <|
                \msSinceEpoch ->
                    msSinceEpoch
                        |> fromMsSinceEpoch
                        |> toMsAfterEpoch
                        |> Expect.equal msSinceEpoch
            ]
        , describe "TimeLapses arithmetic should be correct."
            [ test "combining milliseconds and seconds" <|
                \() ->
                    milliseconds 23
                        |> and seconds 5
                        |> timeLapseView
                        |> Expect.equal { milliseconds = 23, seconds = 5, minutes = 0, hours = 0 }
            , test "combining overlaping milliseconds and seconds" <|
                \() ->
                    milliseconds 1015
                        |> and seconds 5
                        |> timeLapseView
                        |> Expect.equal { milliseconds = 15, seconds = 6, minutes = 0, hours = 0 }
            , test "combining overlaping milliseconds, seconds, minutes and hours" <|
                \() ->
                    milliseconds 100015
                        |> and seconds 75
                        |> and minutes 190
                        |> and hours 20
                        |> timeLapseView
                        |> Expect.equal { milliseconds = 15, seconds = 55, minutes = 12, hours = 23 }
            , fuzz2 fuzzMoment fuzzTimeLapse "elapsed should return a timeLapse that gives the moment if we move it into the future" <|
                \moment timeLapse ->
                    moment
                        |> intoFuture timeLapse
                        |> elapsed moment
                        |> Expect.equal ( timeLapse, IntoTheFuture )
            , fuzz2 fuzzMoment fuzzNonZeroTimeLapse "elapsed should return a timeLapse that gives the moment if we move it into the past" <|
                \moment timeLapse ->
                    moment
                        |> intoPast timeLapse
                        |> elapsed moment
                        |> Expect.equal ( timeLapse, IntoThePast )
            ]
        , describe "Moving Moments with TimeLapses."
            [ fuzz2 fuzzMoment fuzzTimeLapse "Moving into future and back into past should return to same moment" <|
                \moment timeLapse ->
                    moment
                        |> intoFuture timeLapse
                        |> intoPast timeLapse
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
        , describe "earliest"
            [ fuzz fuzzMoment "should return the first moment if it is earlier" <|
                \earlierMoment ->
                    let
                        laterMoment =
                            intoFuture (seconds 1) earlierMoment
                    in
                    earliest earlierMoment laterMoment
                        |> Expect.equal earlierMoment
            , fuzz fuzzMoment "should return the second moment if it is later" <|
                \laterMoment ->
                    let
                        earlierMoment =
                            intoPast (seconds 1) laterMoment
                    in
                    earliest laterMoment earlierMoment
                        |> Expect.equal earlierMoment
            ]
        , describe "chronologicalComparison"
            [ fuzz2 fuzzMoment fuzzTimeLapse "should always put the first moment first" <|
                \earlierMoment timeLapse ->
                    let
                        laterMoment =
                            intoFuture timeLapse earlierMoment
                    in
                    List.sortWith chronologicalComparison [ laterMoment, earlierMoment ]
                        |> Expect.equalLists [ earlierMoment, laterMoment ]
            , fuzz3 fuzzMoment fuzzMoment fuzzMoment "later moment should be in the future" <|
                \moment1 moment2 moment3 ->
                    let
                        moments =
                            List.sortWith chronologicalComparison [ moment1, moment2, moment3 ]

                        expectIntoFuture earlierMoment laterMoment =
                            elapsed earlierMoment laterMoment
                                |> Tuple.second
                                |> Expect.equal IntoTheFuture

                        later list =
                            List.foldl (\moment ( i, expects ) -> ( moment, expectIntoFuture i moment :: expects )) ( fromMsSinceEpoch (-2 ^ 31), [] ) list
                                |> Tuple.second
                                |> List.map always
                    in
                    Expect.all (later moments) moments
            ]
        ]
