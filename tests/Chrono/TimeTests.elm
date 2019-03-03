module Chrono.TimeTests exposing (all)

import Chrono.Moment as Moment
import Chrono.Time exposing (..)
import Chrono.TimeZone as TimeZone
import Expect
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
        ]
