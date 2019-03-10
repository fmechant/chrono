module Chrono.GregorianCalendar exposing
    ( Duration
    , Month(..)
    , andThen
    , days
    , fromDayMonthYear
    , fromMonthNumber
    , intoFuture
    , intoPast
    , months
    , toDayMonthYear
    , toMonthNumber
    , years
    )

import Chrono.Date as Date exposing (Date)



---- Conversion


{-| Convert a year, month and day on the gregorian calendar to a date.

It uses the algoritm described in <https://en.wikipedia.org/wiki/Julian_day>

-}
fromDayMonthYear : { day : Int, month : Month, year : Int } -> Date.Date
fromDayMonthYear { day, month, year } =
    let
        m =
            toMonthNumber month
    in
    Date.fromJDN <| (1461 * (year + 4800 + (m - 14) // 12)) // 4 + (367 * (m - 2 - 12 * ((m - 14) // 12))) // 12 - (3 * ((year + 4900 + (m - 14) // 12) // 100)) // 4 + day - 32075


{-| Convert a date to the year, month and day on the gregorian calendar.
-}
toDayMonthYear : Date -> { day : Int, month : Month, year : Int }
toDayMonthYear date =
    let
        j =
            Date.toJDN date

        f =
            j + 1401 + (((4 * j + 274277) // 146097) * 3) // 4 - 38

        e =
            4 * f + 3

        g =
            modBy 1461 e // 4

        h =
            5 * g + 2

        d =
            modBy 153 h // 5 + 1

        m =
            modBy 12 (h // 153 + 2) + 1

        y =
            (e // 1461) - 4716 + (12 + 2 - m) // 12
    in
    { day = d, month = fromMonthNumber m, year = y }



---- Month


{-| The month in the gregorian calendar.
-}
type Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December


{-| Convert the month to a number representing the month.
1 is January, 12 is December.
-}
toMonthNumber : Month -> Int
toMonthNumber month =
    case month of
        January ->
            1

        February ->
            2

        March ->
            3

        April ->
            4

        May ->
            5

        June ->
            6

        July ->
            7

        August ->
            8

        September ->
            9

        October ->
            10

        November ->
            11

        December ->
            12


{-| Convert the month number to a month.
1 is January, 12 is December.
-}
fromMonthNumber : Int -> Month
fromMonthNumber number =
    case modBy 12 number of
        1 ->
            January

        2 ->
            February

        3 ->
            March

        4 ->
            April

        5 ->
            May

        6 ->
            June

        7 ->
            July

        8 ->
            August

        9 ->
            September

        10 ->
            October

        11 ->
            November

        _ ->
            December



---- Move ----


{-| Move the date a duration into the future.
-}
intoFuture : Duration -> Date -> Date
intoFuture (Duration durations) date =
    durations
        |> List.foldl move date


intoPast : Duration -> Date -> Date
intoPast (Duration durations) date =
    durations
        |> List.map negate
        |> List.foldl move date


move : DurationItem -> Date -> Date
move item date =
    case item of
        NumberOfDays noDays ->
            date
                |> Date.intoFuture (Date.days noDays)

        NumberOfMonths noMonths ->
            let
                dmy =
                    toDayMonthYear date

                monthsWithoutYears =
                    remainderBy 12 noMonths

                aPrioriYears =
                    noMonths // 12

                newMonthMinusOne =
                    toMonthNumber dmy.month + monthsWithoutYears - 1

                yearsDiffWhenPositieve =
                    newMonthMinusOne // 12

                yearDiff =
                    if newMonthMinusOne < 0 then
                        yearsDiffWhenPositieve - 1

                    else
                        yearsDiffWhenPositieve
            in
            fromDayMonthYear
                { day = dmy.day
                , month = fromMonthNumber <| 1 + modBy 12 newMonthMinusOne
                , year = dmy.year + yearDiff + aPrioriYears
                }

        NumberOfYears noYears ->
            let
                dmy =
                    toDayMonthYear date
            in
            fromDayMonthYear { day = dmy.day, month = dmy.month, year = dmy.year + noYears }


negate : DurationItem -> DurationItem
negate item =
    case item of
        NumberOfDays value ->
            NumberOfDays -value

        NumberOfMonths value ->
            NumberOfMonths -value

        NumberOfYears value ->
            NumberOfYears -value



---- DURATION ----


{-| Duration represents a laps of time. It is represented in the date and time model,
because we are thinking about actual elaps of specific days, months and years.
-}
type Duration
    = Duration (List DurationItem)


type DurationItem
    = NumberOfDays Int
    | NumberOfMonths Int
    | NumberOfYears Int


days : Int -> Duration
days value =
    Duration [ NumberOfDays value ]


months : Int -> Duration
months value =
    Duration [ NumberOfMonths value ]


years : Int -> Duration
years value =
    Duration [ NumberOfYears value ]


{-| Combine two durations.

It has an odd signiture to be able to efficiently use it using the pipe (|>) operator.
Example:

    years 2
        |> andThen days 15
        |> viewDuration
    --> { days: 15, months: 0, years: 2}

-}
andThen : (Int -> Duration) -> Int -> Duration -> Duration
andThen fct value (Duration durations) =
    let
        (Duration toAdd) =
            fct value
    in
    Duration (durations ++ toAdd)
