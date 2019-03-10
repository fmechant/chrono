module Chrono.GregorianCalendarTests exposing (all)

import Chrono.Date as Date exposing (Date)
import Chrono.GregorianCalendar exposing (..)
import Chrono.Moment as Moment
import Expect
import Fuzz
import Random
import Test exposing (..)
import Time


all : Test
all =
    describe "Gregorian Calendar Tests"
        [ describe "conversions"
            [ test "fromDayMonthYear of January 1, 2000 in utc is the correct date. " <|
                \() ->
                    fromDayMonthYear { year = 2000, month = January, day = 1 }
                        |> Expect.equal firstJanuary2000
            , test "toDayMonthYear returns the correct day, month and year. " <|
                \() ->
                    twentyEightFebruary2000
                        |> toDayMonthYear
                        |> Expect.equal { year = 2000, month = February, day = 28 }
            ]
        , describe "leap years"
            [ test "2000 is a leap year" <|
                \() ->
                    isLeapYear 2000
                        |> Expect.equal True
            , test "2017 is a common year" <|
                \() ->
                    isLeapYear 2017
                        |> Expect.equal False
            , test "2100 is a common year" <|
                \() ->
                    isLeapYear 2100
                        |> Expect.equal False
            , test "2400 is a leap year" <|
                \() ->
                    isLeapYear 2400
                        |> Expect.equal True
            ]
        , describe "moving dates"
            [ test "into the future returns the correct date" <|
                \() ->
                    firstJanuary2000
                        |> intoFuture (days 58)
                        |> Expect.equal twentyEightFebruary2000
            , fuzz fuzzDate "0 months in the future is the same date" <|
                \aDate ->
                    aDate
                        |> intoFuture (months 0 stayInSameMonth)
                        |> Expect.equal aDate
            , test "takes the order of the duration into account" <|
                \() ->
                    twentyEightFebruary2000
                        |> intoFuture (days 3 |> andThen (months 3 stayInSameMonth))
                        |> Expect.equal (fromDayMonthYear { day = 2, month = June, year = 2000 })
            , test "moving to 30 February stays in same month and becomes 28 February for common year" <|
                \() ->
                    fromDayMonthYear { day = 30, month = January, year = 2003 }
                        |> intoFuture (months 1 stayInSameMonth)
                        |> Expect.equal (fromDayMonthYear { day = 28, month = February, year = 2003 })
            , test "moving to 30 February stays in same month and becomes 29 February for leap year" <|
                \() ->
                    fromDayMonthYear { day = 30, month = January, year = 2004 }
                        |> intoFuture (months 1 stayInSameMonth)
                        |> Expect.equal (fromDayMonthYear { day = 29, month = February, year = 2004 })
            ]
        ]


firstJanuary2000 : Date
firstJanuary2000 =
    Date.fromJDN 2451545


twentyEightFebruary2000 : Date
twentyEightFebruary2000 =
    Date.fromJDN (2451545 + 58)


fuzzDate : Fuzz.Fuzzer Date
fuzzDate =
    Fuzz.map Date.fromJDN <| Fuzz.intRange 2415021 3415021


fuzzDuration : Fuzz.Fuzzer Duration
fuzzDuration =
    Fuzz.oneOf
        [ Fuzz.map days <| Fuzz.intRange 0 Random.maxInt
        , Fuzz.map2 months (Fuzz.intRange 0 1000) (Fuzz.constant stayInSameMonth)
        , Fuzz.map2 years (Fuzz.intRange 0 40) (Fuzz.constant stayInSameMonth)
        ]
