module Chrono.DateTests exposing (all)

import Chrono.Date exposing (..)
import Chrono.Moment as Moment
import Chrono.Time as Time
import Expect
import Test exposing (..)


all : Test
all =
    describe "Date tests"
        [ describe "fromMoment should use correct conversion to JDN."
            [ test "Epoch moment is JDN 2,440,588 in utc." <|
                \() ->
                    Moment.fromMsSinceEpoch 0
                        |> fromMoment Time.utc
                        |> toJDN
                        |> Expect.equal 2440588
            , test "January 1, 2000 in utc is JDN 2,451,545." <|
                \() ->
                    Moment.fromMsSinceEpoch 946684800000
                        |> fromMoment Time.utc
                        |> toJDN
                        |> Expect.equal 2451545
            , test "Epoch moment is JDN 2,440,587 in GMT-5" <|
                \() ->
                    Moment.fromMsSinceEpoch 0
                        |> fromMoment (Time.customZone (-5 * 60) [])
                        |> toJDN
                        |> Expect.equal 2440587
            ]
        ]
