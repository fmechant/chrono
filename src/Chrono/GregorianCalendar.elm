module Chrono.GregorianCalendar exposing
    ( Duration
    , Month(..)
    , YearType(..)
    , andThen
    , days
    , fromGregorianDate
    , fromMonthNumber
    , intoFuture
    , intoPast
    , months
    , stayInSameMonth
    , toGregorianDate
    , toMonthNumber
    , toYearType
    , typeOfYear
    , years
    )

import Chrono.Date as Date exposing (Date)



---- Conversion


{-| The date specified using the gregorian year, month and day.

Use it with fromGregorianDate to conveniently create a date.

Use the actual Date type to store, transmit, move, ... dates.

-}
type alias GregorianDate =
    { year : Int, month : Month, day : Int }


{-| The move strategy defines what to do if the move provides an invalid gregorian date.

For example, what to do when 3 months are added to 2019-01-31?
The strategy defines if the result is 2019-04-30 or 2019-05-01 or ...

-}
type alias MoveStrategy =
    GregorianDate -> GregorianDate


{-| Convert a year, month and day on the gregorian calendar to a date.

Year, month and day combinations that are not actual gregorian dates, like 2019-04-31 give unpredictable results.

It uses the algoritm described in <https://en.wikipedia.org/wiki/Julian_day>

-}
fromGregorianDate : GregorianDate -> Date
fromGregorianDate { day, month, year } =
    let
        m =
            toMonthNumber month
    in
    Date.fromJDN <| (1461 * (year + 4800 + (m - 14) // 12)) // 4 + (367 * (m - 2 - 12 * ((m - 14) // 12))) // 12 - (3 * ((year + 4900 + (m - 14) // 12) // 100)) // 4 + day - 32075


{-| Convert a date to the year, month and day on the gregorian calendar.

This is typically used in the view to visualize the date.

-}
toGregorianDate : Date -> GregorianDate
toGregorianDate date =
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


{-| When confronted with impossible dates, moves to the closest valid day in the same month.
Typically used when defining a duration of months or years.
-}
stayInSameMonth : MoveStrategy
stayInSameMonth dmy =
    let
        maxDay =
            numberOfDaysInMonth dmy.month dmy.year
    in
    { day = clamp 1 maxDay dmy.day, month = dmy.month, year = dmy.year }



---- Type of Year


type YearType
    = CommonYear
    | LeapYear


{-| Is the given year a leap year or a common year.
-}
typeOfYear : Int -> YearType
typeOfYear year =
    if (modBy 4 year == 0) && (modBy 100 year /= 0 || modBy 400 year == 0) then
        LeapYear

    else
        CommonYear


{-| What is the type of year of the date, common year of leap year?
-}
toYearType : Date -> YearType
toYearType date =
    toGregorianDate date |> .year |> typeOfYear



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


{-| The total number of days in the month, in that year.
-}
numberOfDaysInMonth : Month -> Int -> Int
numberOfDaysInMonth month year =
    case month of
        January ->
            31

        February ->
            case typeOfYear year of
                LeapYear ->
                    29

                CommonYear ->
                    28

        March ->
            31

        April ->
            30

        May ->
            31

        June ->
            30

        July ->
            31

        August ->
            31

        September ->
            30

        October ->
            31

        November ->
            30

        December ->
            31



---- Move ----


{-| Move the date a duration into the future.
-}
intoFuture : Duration -> Date -> Date
intoFuture (Duration durations) date =
    durations
        |> List.foldl move date


{-| Move the date a duration into the past.
-}
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

        NumberOfMonths noMonths strategy ->
            let
                dmy =
                    toGregorianDate date

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
            fromGregorianDate <|
                strategy
                    { day = dmy.day
                    , month = fromMonthNumber <| 1 + modBy 12 newMonthMinusOne
                    , year = dmy.year + yearDiff + aPrioriYears
                    }

        NumberOfYears noYears strategy ->
            let
                dmy =
                    toGregorianDate date
            in
            fromGregorianDate <| strategy { day = dmy.day, month = dmy.month, year = dmy.year + noYears }


negate : DurationItem -> DurationItem
negate item =
    case item of
        NumberOfDays value ->
            NumberOfDays -value

        NumberOfMonths value strategy ->
            NumberOfMonths -value strategy

        NumberOfYears value strategy ->
            NumberOfYears -value strategy



---- DURATION ----


{-| Duration represents a laps of time. It is represented in the date and time model,
because we are thinking about actual elaps of specific days, months and years.
-}
type Duration
    = Duration (List DurationItem)


type DurationItem
    = NumberOfDays Int
    | NumberOfMonths Int MoveStrategy
    | NumberOfYears Int MoveStrategy


days : Int -> Duration
days value =
    Duration [ NumberOfDays value ]


months : Int -> MoveStrategy -> Duration
months value strategy =
    Duration [ NumberOfMonths value strategy ]


years : Int -> MoveStrategy -> Duration
years value strategy =
    Duration [ NumberOfYears value strategy ]


{-| Combine two durations.

Example:

    fromDayMonthYear { day = 1, month = January, year = 2000  }
        |> intoFuture (days 15 |> andThen (years 2 stayInSameMonth))
    --> fromDayMonthYear { day = 16, month = January, year = 2002  }

-}
andThen : Duration -> Duration -> Duration
andThen (Duration toAdd) (Duration durations) =
    Duration (durations ++ toAdd)
