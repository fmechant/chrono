module Chrono.DateTests exposing (all)

import Chrono.Date exposing (..)
import Chrono.Moment as Moment
import Chrono.Time
import Expect
import Test exposing (..)
import Time


all : Test
all =
    describe "Date tests"
        [ describe "fromMoment should use correct conversion to JDN."
            [ test "Epoch moment is JDN 2,440,588 in utc." <|
                \() ->
                    Moment.fromMsSinceEpoch 0
                        |> fromMoment Chrono.Time.utc
                        |> toJDN
                        |> Expect.equal 2440588
            , test "January 1, 2000 in utc is JDN 2,451,545." <|
                \() ->
                    Moment.fromMsSinceEpoch 946684800000
                        |> fromMoment Chrono.Time.utc
                        |> toJDN
                        |> Expect.equal 2451545
            , test "Epoch moment is JDN 2,440,587 in GMT-5" <|
                \() ->
                    Moment.fromMsSinceEpoch 0
                        |> fromMoment (Chrono.Time.customZone (-5 * 60) [])
                        |> toJDN
                        |> Expect.equal 2440587
            ]
        , describe "toWeekday"
            [ test "Epoch moment is a Thursday." <|
                \() ->
                    Moment.fromMsSinceEpoch 0
                        |> fromMoment Chrono.Time.utc
                        |> toWeekday
                        |> Expect.equal Time.Thu
            ]
        ]
