module Chrono.GregorianCalendar exposing
    ( toDate, fromDate
    , Month(..), toMonthNumber, fromMonthNumber
    , Moves, travel, andThen, onlyWhen, andThenOnlyWhen, MoveStrategy, stayInSameMonth
    , intoFuture, intoPast, days, weeks, months, years
    , nextWeekday, lastWeekday, toDayInMonth
    , inMonth, Ordinal(..), first, second, third, last, secondToLast
    , YearType(..), typeOfYear, toYearType
    )

{-| Module for working with Dates using the GregorianCalendar.

It contains the concepts specific for the Gregorian calendar, like months,
years.

It enables complex time travel by defining moves, like:

  - go to the next Wednesday,
  - and then go to the next month,
  - and then if the month is September, go two months in the past,
  - and then take the last day of the month.


# Collecting Dates with Time Travel

Using these moves with `Date.collect` is really empowering.
Suppose you want the last three Wednesdays of the month of a certain date.

    import Chrono.Date as Date exposing (Date, Weekday(..))

    may2020 : Int -> Date
    may2020 day =
        toDate { year = 2020, month = May, day = day }

    let
        lastWednesday =
            may2020 22
                |> travel (inMonth last Wednesday )
    in
    lastWednesday
        |> Date.collect 2 (travel <| lastWeekday Wednesday)
        |> (::) lastWednesday
        |> List.sortWith Date.chronologicalDateComparison
        --> [ may2020 13, may2020 20, may2020 27]

Or, maybe you need the next 10 weekdays?

    import Chrono.Date as Date exposing (Date, Weekday(..))

    let
        nextWeekday =
            intoFuture (days 1)
                |> andThenOnlyWhen (Date.toWeekday >> (==) Saturday) (intoFuture (days 2))
                |> andThenOnlyWhen (Date.toWeekday >> (==) Sunday) (intoFuture (days 1))
    in
    may2020 2
        |> Date.collect 10 (travel nextWeekday)
        --> [ may2020 4, may2020 5, may2020 6, may2020 7, may2020 8, may2020 11, may2020 12, may2020 13, may2020 14, may2020 15 ]


# Create Dates

@docs toDate, fromDate


# Months

@docs Month, toMonthNumber, fromMonthNumber, numberOfDaysInMonth


# Time Travel

@docs Moves, travel, andThen, onlyWhen, andThenOnlyWhen, MoveStrategy, stayInSameMonth


## Nominal Moves

Nominal moves are moves of a number of days, weeks, months, years.

@docs NominalMove, intoFuture, intoPast, days, weeks, months, years, reverse


## Ordinal Moves

Ordinal moves are moves to the next weekday, or day of the month, or previous.

@docs nextWeekday, lastWeekday, toDayInMonth

@docs inMonth, Ordinal, first, second, third, last, secondToLast


# Leap Years

@docs YearType, typeOfYear, toYearType

-}

import Chrono.Date as Date exposing (Date)



---- Conversion


{-| The date specified using the gregorian year, month and day.

Use it with toDate to conveniently create a date.

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
toDate : GregorianDate -> Date
toDate { day, month, year } =
    let
        m =
            toMonthNumber month
    in
    Date.fromJDN <| (1461 * (year + 4800 + (m - 14) // 12)) // 4 + (367 * (m - 2 - 12 * ((m - 14) // 12))) // 12 - (3 * ((year + 4900 + (m - 14) // 12) // 100)) // 4 + day - 32075


{-| Convert a date to the year, month and day on the gregorian calendar.

This is typically used in the view to visualize the date.
Avoid using this for calculations, let this library do the hard work for you.

-}
fromDate : Date -> GregorianDate
fromDate date =
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
It always returns a valid GregorianDate.

Typically used when defining a move of months or years.

Example:

    { day = 50, month = January, year = 2000  }
        |> stayInSameMonth
    --> { day = 31, month = January, year = 2000  }

-}
stayInSameMonth : MoveStrategy
stayInSameMonth dmy =
    let
        maxDay =
            numberOfDaysInMonth dmy.month dmy.year
    in
    { day = clamp 1 maxDay dmy.day, month = dmy.month, year = dmy.year }



---- Type of Year


{-| The type of year, common or leap.
-}
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
    fromDate date |> .year |> typeOfYear



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

Any negative integer and any integer above 12 is mapped to December.

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



---- Time Travel ----


{-| Moves represents specific moves you want to make starting from a date.
Moves is different from the Duration in Date or Moment, because a duration makes little
sense here. How long is a Month? It depends. That is why we prefer here to use
moves instead of a duration that depends on what date you start from.

The order of the moves matter. There is no associativity here.

-}
type Moves
    = Moves (List Move)


{-| The possible moves to make
-}
type Move
    = ToFuture NominalMove
    | ToPast NominalMove
    | ToNextWeekDay Date.Weekday
    | ToLastWeekDay Date.Weekday
    | InMonth Ordinal Date.Weekday
    | ToDayInMonth Int
    | Predicate (Date -> Bool) Moves


{-| Execute the time travel, as defined in the Moves, starting from the date.
-}
travel : Moves -> Date -> Date
travel (Moves moves) date =
    moves
        |> List.foldl move date


{-| Combine two moves.

Example:

    toDate { day = 1, month = January, year = 2000  }
        |> travel (intoFuture (days 15) |> andThen intoFuture (years 2 stayInSameMonth))
    --> toDate { day = 16, month = January, year = 2002  }

-}
andThen : (a -> Moves) -> a -> Moves -> Moves
andThen fct value (Moves moves) =
    let
        (Moves movesToAppend) =
            fct value
    in
    Moves (moves ++ movesToAppend)


{-| Only perform the moves if the predicate for the date is valid.

Example:

    import Chrono.Date as Date

    let
        weekday = Date.Wednesday
    in
    toDate { day = 5, month = January, year = 2000  }
        |> travel (onlyWhen (\d -> Date.toWeekday d /= weekday) (nextWeekday weekday))
    --> toDate { day = 5, month = January, year = 2000  }

-}
onlyWhen : (Date -> Bool) -> Moves -> Moves
onlyWhen fct movesIfTrue =
    Moves [ Predicate fct movesIfTrue ]


{-| Combination of andThen and onlyWhen.

Example:

    import Chrono.Date as Date

    let
        weekday = Date.Wednesday
        moves =
            toDayInMonth 1
                |> andThenOnlyWhen (\d -> Date.toWeekday d /= weekday) (nextWeekday weekday)
    in
    toDate { day = 27, month = March, year = 2000  }
        |> travel moves
    --> toDate { day = 1, month = March, year = 2000  }

-}
andThenOnlyWhen : (Date -> Bool) -> Moves -> Moves -> Moves
andThenOnlyWhen fct movesIfTrue (Moves moves) =
    Moves (moves ++ [ Predicate fct movesIfTrue ])


{-| Travel a single move from the date.
-}
move : Move -> Date -> Date
move item date =
    case item of
        Predicate fct moveIfTrue ->
            if fct date then
                travel moveIfTrue date

            else
                date

        ToPast nominalMove ->
            move (ToFuture (reverse nominalMove)) date

        ToFuture (NumberOfDays noDays) ->
            date
                |> Date.intoFuture (Date.days noDays)

        ToFuture (NumberOfWeeks noWeeks) ->
            date
                |> Date.intoFuture (Date.weeks noWeeks)

        ToFuture (NumberOfMonths noMonths strategy) ->
            let
                dmy =
                    fromDate date

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
            toDate <|
                strategy
                    { day = dmy.day
                    , month = fromMonthNumber <| 1 + modBy 12 newMonthMinusOne
                    , year = dmy.year + yearDiff + aPrioriYears
                    }

        ToFuture (NumberOfYears noYears strategy) ->
            let
                dmy =
                    fromDate date
            in
            toDate <| strategy { day = dmy.day, month = dmy.month, year = dmy.year + noYears }

        ToNextWeekDay weekday ->
            date
                |> Date.next weekday

        ToLastWeekDay weekday ->
            date
                |> Date.last weekday

        InMonth (Th th) weekday ->
            let
                moves =
                    toDayInMonth 1
                        |> andThenOnlyWhen (\d -> Date.toWeekday d /= weekday) (nextWeekday weekday)
                        |> andThen intoFuture (weeks (th - 1))
            in
            travel moves date

        InMonth (ThToLast th) weekday ->
            let
                moves =
                    toDayInMonth 1
                        |> andThen intoFuture (months 1 stayInSameMonth)
                        |> andThen lastWeekday weekday
                        |> andThen intoPast (weeks (th - 1))
            in
            travel moves date

        ToDayInMonth day ->
            let
                dmy =
                    fromDate date
            in
            toDate <| stayInSameMonth { day = day, month = dmy.month, year = dmy.year }



---- Nominal Moves ----


{-| A Nominal Move is a number of days, weeks, months or years.
When moving months or years, we need a Move Strategy. See MoveStrategy for more info.
-}
type NominalMove
    = NumberOfDays Int
    | NumberOfWeeks Int
    | NumberOfMonths Int MoveStrategy
    | NumberOfYears Int MoveStrategy


{-| Moves that define a nominal move into the future.

Example:

    toDate { day = 1, month = January, year = 2000  }
        |> travel (intoFuture (days 15))
    --> toDate { day = 16, month = January, year = 2000  }

-}
intoFuture : NominalMove -> Moves
intoFuture =
    ToFuture >> List.singleton >> Moves


{-| Moves that define a nominal move into the past.
Example:

    toDate { day = 16, month = January, year = 2000  }
        |> travel (intoPast (days 15))
    --> toDate { day = 1, month = January, year = 2000  }

-}
intoPast : NominalMove -> Moves
intoPast =
    ToPast >> List.singleton >> Moves


{-| A nominal move of a certain number of days.
It needs intoFuture or intoPast to actually enable time travel.
-}
days : Int -> NominalMove
days value =
    NumberOfDays value


{-| A nominal move of a certain number of weeks.
It needs intoFuture or intoPast to actually enable time travel.
-}
weeks : Int -> NominalMove
weeks value =
    NumberOfWeeks value


{-| A nominal move of a certain number of months.
See MoveStrategy to understand why it is needed.
It needs intoFuture or intoPast to actually enable time travel.
-}
months : Int -> MoveStrategy -> NominalMove
months value strategy =
    NumberOfMonths value strategy


{-| A nominal move of a certain number of years.
See MoveStrategy to understand why it is needed.
It needs intoFuture or intoPast to actual enable time travel.
-}
years : Int -> MoveStrategy -> NominalMove
years value strategy =
    NumberOfYears value strategy


{-| Reverse the nominal move
-}
reverse : NominalMove -> NominalMove
reverse item =
    case item of
        NumberOfDays value ->
            NumberOfDays -value

        NumberOfWeeks value ->
            NumberOfWeeks -value

        NumberOfMonths value strategy ->
            NumberOfMonths -value strategy

        NumberOfYears value strategy ->
            NumberOfYears -value strategy



---- Move Ordinally ----


{-| Moves that define a move to the next day that is the weekday.
It always moves into the future.

See onlyWhen for deciding to not move if the day is already the weekday.

Example:

    import Chrono.Date as Date

    toDate { day = 1, month = January, year = 2000 }
        |> travel (nextWeekday Date.Wednesday)
    --> toDate { day = 5, month = January, year = 2000 }

-}
nextWeekday : Date.Weekday -> Moves
nextWeekday =
    ToNextWeekDay >> List.singleton >> Moves


{-| Moves that define a move to the last day that is the weekday.
It always moves into the past.

See onlyWhen for deciding to not move if the day is already a weekday.

Example:

    import Chrono.Date as Date

    toDate { day = 8, month = January, year = 2000 }
        |> travel (lastWeekday Date.Wednesday)
    --> toDate { day = 5, month = January, year = 2000 }

-}
lastWeekday : Date.Weekday -> Moves
lastWeekday =
    ToLastWeekDay >> List.singleton >> Moves


{-| Moves that define a move to a specific day in the current month.
The day is clamped so that it is a valid day in the month.

Example:

    toDate { day = 8, month = January, year = 2000 }
        |> travel (toDayInMonth 15)
    --> toDate { day = 15, month = January, year = 2000 }

-}
toDayInMonth : Int -> Moves
toDayInMonth =
    ToDayInMonth >> List.singleton >> Moves


{-| Moves that define a move to a specific weekday in the current month.

Example:

    import Chrono.Date as Date

    toDate { day = 8, month = January, year = 2000 }
        |> travel (inMonth second Date.Wednesday)
    --> toDate { day = 12, month = January, year = 2000 }

-}
inMonth : Ordinal -> Date.Weekday -> Moves
inMonth ordinal weekday =
    Moves [ InMonth ordinal weekday ]


{-| An ordinal. Can be the 1st, 2nd, 3rd, 4th, ... or the last, the second to last, ...
-}
type Ordinal
    = Th Int
    | ThToLast Int


{-| The ordinal for the first occurence
-}
first : Ordinal
first =
    Th 1


{-| The ordinal for the second occurence
-}
second : Ordinal
second =
    Th 2


{-| The ordinal for the third occurence.

Higher that this, use the Th constructor, like in fifth = Th 5.

-}
third : Ordinal
third =
    Th 3


{-| The ordinal for the last occurence.
-}
last : Ordinal
last =
    ThToLast 1


{-| The ordinal for the second to last occurence.

Higher that this, use the ThToLast constructor, like in fifthToLast = ThToLast 5.

-}
secondToLast : Ordinal
secondToLast =
    ThToLast 2
