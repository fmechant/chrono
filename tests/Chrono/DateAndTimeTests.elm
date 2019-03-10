module Chrono.DateAndTimeTests exposing (all)

import Chrono.Date as Date
import Chrono.DateAndTime exposing (..)
import Chrono.Moment as Moment
import Chrono.Time as Time
import Chrono.TimeZone as TimeZone
import Expect
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
                            TimeZone.utc
                    in
                    moment
                        |> fromMoment timeZone
                        |> Expect.equal { date = Date.fromMoment timeZone moment, time = Time.fromMoment timeZone moment }
            ]
        ]
