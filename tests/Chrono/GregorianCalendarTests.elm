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
            [ test "toDate of January 1, 2000 in utc is the correct date. " <|
                \() ->
                    toDate { year = 2000, month = January, day = 1 }
                        |> Expect.equal firstJanuary2000
            , test "fromDate returns the correct day, month and year. " <|
                \() ->
                    twentyEightFebruary2000
                        |> fromDate
                        |> Expect.equal { year = 2000, month = February, day = 28 }
            , fuzz fuzzDate "round trip through fromDate, toDate should return same date" <|
                \aDate ->
                    aDate
                        |> fromDate
                        |> toDate
                        |> Expect.equal aDate
            , fuzz3 (Fuzz.intRange 1 31) fuzzMonth fuzzYear "round trip through toDate, fromDate should return same Gregorian date" <|
                \aDay aMonth aYear ->
                    let
                        aValidDate =
                            { year = aYear, month = aMonth, day = aDay }
                                |> stayInSameMonth
                    in
                    aValidDate
                        |> toDate
                        |> fromDate
                        |> Expect.equal aValidDate
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
                    toDate { year = 2019, month = May, day = 6 }
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
                , fuzz2 fuzzDate (Fuzz.intRange 1 100) "moving weeks into the future goes 7 times that in days forward" <|
                    \aDate numberOfWeeks ->
                        aDate
                            |> travel (intoFuture (weeks numberOfWeeks))
                            |> Expect.equal (Date.intoFuture (Date.days (7 * numberOfWeeks)) aDate)
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
                            |> Expect.equal (toDate { day = 2, month = June, year = 2000 })
                , test "moving to 30 February stays in same month and becomes 28 February for common year" <|
                    \() ->
                        toDate { day = 30, month = January, year = 2003 }
                            |> travel (intoFuture (months 1 stayInSameMonth))
                            |> Expect.equal (toDate { day = 28, month = February, year = 2003 })
                , test "moving to 30 February stays in same month and becomes 29 February for leap year" <|
                    \() ->
                        toDate { day = 30, month = January, year = 2004 }
                            |> travel (intoFuture (months 1 stayInSameMonth))
                            |> Expect.equal (toDate { day = 29, month = February, year = 2004 })
                , test "moving months into the next year changes the year" <|
                    \() ->
                        toDate { day = 14, month = October, year = 2005 }
                            |> travel (intoFuture (months 5 stayInSameMonth))
                            |> Expect.equal (toDate { day = 14, month = March, year = 2006 })
                , fuzz3 (Fuzz.intRange 1 28) fuzzMonth (Fuzz.tuple ( fuzzYear, Fuzz.intRange 1 100 )) "moving years" <|
                    \aDay aMonth ( aYear, numberOfYears ) ->
                        toDate { year = aYear, month = aMonth, day = aDay }
                            |> travel (intoFuture (years numberOfYears stayInSameMonth))
                            |> Expect.equal (toDate { year = aYear + numberOfYears, month = aMonth, day = aDay })
                ]
            , describe "traveling into the past" <|
                [ fuzz2 (Fuzz.intRange 1 31) (Fuzz.intRange 1900 2100) "moving a month into the past in January goes to the year before" <|
                    \aDay aYear ->
                        toDate { year = aYear, month = January, day = aDay }
                            |> travel (intoPast (months 1 stayInSameMonth))
                            |> Expect.equal (toDate { year = aYear - 1, month = December, day = aDay })
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
                            |> Expect.equal (toDate { day = 5, month = January, year = 2000 })
                , test "second Wednesday in month returns the second Wednesday of the month" <|
                    \() ->
                        firstJanuary2000
                            |> travel (inMonth second Date.Wednesday)
                            |> Expect.equal (toDate { day = 12, month = January, year = 2000 })
                , test "second to last Wednesday in month returns the second to last Wednesday of the month" <|
                    \() ->
                        firstJanuary2000
                            |> travel (inMonth secondToLast Date.Wednesday)
                            |> Expect.equal (toDate { day = 19, month = January, year = 2000 })
                , test "specific day in month returns the day of the month" <|
                    \() ->
                        firstJanuary2000
                            |> travel (toDayInMonth 16)
                            |> Expect.equal (toDate { day = 16, month = January, year = 2000 })
                , test "specific day in month respects the length of the month" <|
                    \() ->
                        toDate { day = 1, month = February, year = 2000 }
                            |> travel (toDayInMonth 31)
                            |> Expect.equal (toDate { day = 29, month = February, year = 2000 })
                ]
            , describe "only when"
                [ fuzz2 fuzzDate (Fuzz.intRange 1 100) "only when should stay in present when predicate returns false" <|
                    \aDate numberOfWeeks ->
                        aDate
                            |> travel (onlyWhen (\_ -> False) (intoFuture (weeks numberOfWeeks)))
                            |> Expect.equal aDate
                , fuzz2 fuzzDate (Fuzz.intRange 1 100) "only when should move when predicate returns true" <|
                    \aDate numberOfWeeks ->
                        aDate
                            |> travel
                                (onlyWhen (\_ -> True) (intoFuture (weeks numberOfWeeks))
                                    |> andThen intoPast (weeks numberOfWeeks)
                                )
                            |> Expect.equal aDate
                , fuzz2 fuzzDate (Fuzz.intRange 1 100) "and then only when should stay in present when predicate returns false" <|
                    \aDate numberOfWeeks ->
                        aDate
                            |> travel
                                (intoFuture (weeks 0)
                                    |> andThenOnlyWhen (\_ -> False) (intoPast (weeks numberOfWeeks))
                                )
                            |> Expect.equal aDate
                , fuzz2 fuzzDate (Fuzz.intRange 1 100) "and then only when should move when predicate returns true" <|
                    \aDate numberOfWeeks ->
                        aDate
                            |> travel
                                (intoFuture (weeks numberOfWeeks)
                                    |> andThenOnlyWhen (\_ -> True) (intoPast (weeks numberOfWeeks))
                                )
                            |> Expect.equal aDate
                ]
            ]
        ]


firstJanuary2000 : Date
firstJanuary2000 =
    Date.fromJDN 2451545


twentyEightFebruary2000 : Date
twentyEightFebruary2000 =
    Date.fromJDN (2451545 + 58)
