module Chrono.GregorianCalendarTests exposing (all)

import Chrono.Date as Date
import Chrono.GregorianCalendar as Cal
import Chrono.Moment as Moment
import Expect
import Test exposing (..)
import Time


all : Test
all =
    describe "toDate returns correct date."
        [ test "toDate of January 1, 2000 in utc is JDN 2,451,545. " <|
            \() ->
                Cal.toDate 2000 Time.Jan 1
                    |> Date.toJDN
                    |> Expect.equal 2451545
        ]
