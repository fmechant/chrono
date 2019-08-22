module Chrono.GregorianCalendarTests exposing (all)

import Chrono.Date as Date exposing (Date)
import Chrono.GregorianCalendar exposing (..)
import Chrono.Moment as Moment
import Chrono.TestUtils exposing (..)
import Expect
import Fuzz
import Random
import Test exposing (..)
import Time


all : Test
all =
    describe "Gregorian Calendar Tests"
        [ describe "conversions"
            [ test "fromGregorianDate of January 1, 2000 in utc is the correct date. " <|
                \() ->
                    fromGregorianDate { year = 2000, month = January, day = 1 }
                        |> Expect.equal firstJanuary2000
            , test "toGregorianDate returns the correct day, month and year. " <|
                \() ->
                    twentyEightFebruary2000
                        |> toGregorianDate
                        |> Expect.equal { year = 2000, month = February, day = 28 }
            ]
        , describe "leap years"
            [ test "2000 is a leap year" <|
                \() ->
                    typeOfYear 2000
                        |> Expect.equal LeapYear
            , test "2017 is a common year" <|
                \() ->
                    typeOfYear 2017
                        |> Expect.equal CommonYear
            , test "2100 is a common year" <|
                \() ->
                    typeOfYear 2100
                        |> Expect.equal CommonYear
            , test "2400 is a leap year" <|
                \() ->
                    typeOfYear 2400
                        |> Expect.equal LeapYear
            , test "2019-05-06 is in a common year" <|
                \() ->
                    fromGregorianDate { year = 2019, month = May, day = 6 }
                        |> toYearType
                        |> Expect.equal CommonYear
            ]
        , describe "time travel"
            [ describe "traveling into the future"
                [ test "into the future returns the correct date" <|
                    \() ->
                        firstJanuary2000
                            |> travel (intoFuture (days 58))
                            |> Expect.equal twentyEightFebruary2000
                , fuzz fuzzDate "0 months in the future is the same date" <|
                    \aDate ->
                        aDate
                            |> travel (intoFuture (months 0 stayInSameMonth))
                            |> Expect.equal aDate
                , test "takes the order of the moves into account" <|
                    \() ->
                        let
                            moves =
                                intoFuture (days 3)
                                    |> andThen intoFuture (months 3 stayInSameMonth)
                        in
                        twentyEightFebruary2000
                            |> travel moves
                            |> Expect.equal (fromGregorianDate { day = 2, month = June, year = 2000 })
                , test "moving to 30 February stays in same month and becomes 28 February for common year" <|
                    \() ->
                        fromGregorianDate { day = 30, month = January, year = 2003 }
                            |> travel (intoFuture (months 1 stayInSameMonth))
                            |> Expect.equal (fromGregorianDate { day = 28, month = February, year = 2003 })
                , test "moving to 30 February stays in same month and becomes 29 February for leap year" <|
                    \() ->
                        fromGregorianDate { day = 30, month = January, year = 2004 }
                            |> travel (intoFuture (months 1 stayInSameMonth))
                            |> Expect.equal (fromGregorianDate { day = 29, month = February, year = 2004 })
                ]
            , fuzz2 fuzzDate (Fuzz.intRange 0 1000) "traveling weeks into the past and then back into future returns to the same date" <|
                \aDate numberOfWeeks ->
                    let
                        moves =
                            intoFuture (weeks numberOfWeeks)
                                |> andThen intoPast (weeks numberOfWeeks)
                    in
                    aDate
                        |> travel moves
                        |> Expect.equal aDate
            , describe "moving date ordinally"
                [ test "first Wednesday in month returns the first Wednesday of the month" <|
                    \() ->
                        firstJanuary2000
                            |> travel (inMonth first Date.Wednesday)
                            |> Expect.equal (fromGregorianDate { day = 5, month = January, year = 2000 })
                , test "second Wednesday in month returns the second Wednesday of the month" <|
                    \() ->
                        firstJanuary2000
                            |> travel (inMonth second Date.Wednesday)
                            |> Expect.equal (fromGregorianDate { day = 12, month = January, year = 2000 })
                , test "second to last Wednesday in month returns the second to last Wednesday of the month" <|
                    \() ->
                        firstJanuary2000
                            |> travel (inMonth secondToLast Date.Wednesday)
                            |> Expect.equal (fromGregorianDate { day = 19, month = January, year = 2000 })
                , test "specific day in month returns the day of the month" <|
                    \() ->
                        firstJanuary2000
                            |> travel (toDayInMonth 16)
                            |> Expect.equal (fromGregorianDate { day = 16, month = January, year = 2000 })
                , test "specific day in month respects the length of the month" <|
                    \() ->
                        fromGregorianDate { day = 1, month = February, year = 2000 }
                            |> travel (toDayInMonth 31)
                            |> Expect.equal (fromGregorianDate { day = 29, month = February, year = 2000 })
                ]
            ]
        ]


firstJanuary2000 : Date
firstJanuary2000 =
    Date.fromJDN 2451545


twentyEightFebruary2000 : Date
twentyEightFebruary2000 =
    Date.fromJDN (2451545 + 58)
