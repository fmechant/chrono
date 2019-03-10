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
        , describe "moving dates"
            [ test "into the future returns the correct date" <|
                \() ->
                    firstJanuary2000
                        |> intoFuture (days 58)
                        |> Expect.equal twentyEightFebruary2000
            , fuzz fuzzDate "0 months in the future is the same date" <|
                \aDate ->
                    aDate
                        |> intoFuture (months 0)
                        |> Expect.equal aDate
            , test "takes the order of the duration into account" <|
                \() ->
                    twentyEightFebruary2000
                        |> intoFuture (days 3 |> andThen months 3)
                        |> Expect.equal (fromDayMonthYear { day = 2, month = June, year = 2000 })
            , fuzz2 fuzzDate fuzzDuration "moving into future and then the same into past is staying here." <|
                \date duration ->
                    date
                        |> intoFuture duration
                        |> intoPast duration
                        |> Expect.equal date
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
        , Fuzz.map months <| Fuzz.intRange 0 1000
        , Fuzz.map years <| Fuzz.intRange 0 40
        ]
