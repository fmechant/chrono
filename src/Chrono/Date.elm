module Chrono.Date exposing
    ( Date
    , Duration
    , Weekday(..)
    , days
    , fromJDN
    , fromMoment
    , intoFuture
    , intoPast
    , last
    , next
    , toJDN
    , toMoment
    , toNoon
    , toWeekday
    , toWeekdayNumber
    , weeks
    )

{-| A date is an abstract understanding of a period of time.
It is independent of a time zone.

Mark that the concept of the date is independent of the Calendar one
uses.

-}

import Chrono.Moment as Moment exposing (Moment)
import Chrono.Time exposing (Time)
import Chrono.TimeZone as TimeZone exposing (TimeZone)
import Task exposing (Task)
import Time as CoreTime


{-| A date is an abstract understanding of a period of time.

The internal representation of the Date is the Julian Day Number.
This is the number of days since the Julian day number 0, which is
Monday, January 1, 4713 BC, proleptic Julian calendar or
November 24, 4714 BC, in the proleptic Gregorian calendar.

-}
type Date
    = JDN Int


{-| A weekday.
-}
type Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saterday
    | Sunday


{-| Get the date at the moment when this task is run and in the time zone
where this task is run.
-}
today : Task x Date
today =
    Task.map2 fromMoment (Task.map TimeZone.withSameOffset CoreTime.here) <|
        Task.map (Moment.fromMsSinceEpoch << CoreTime.posixToMillis) CoreTime.now


{-| Find out the date at this moment, in this time zone.
-}
fromMoment : TimeZone -> Moment -> Date
fromMoment zone moment =
    let
        shiftedPosix =
            Moment.toMsAfterEpoch <|
                Moment.intoFutureForZone zone moment

        positiveJdn =
            shiftedPosix // 86400000 + 2440588

        jdn =
            if shiftedPosix < 0 then
                positiveJdn - 1

            else
                positiveJdn
    in
    JDN jdn


{-| Convert the date and time, in this time zone, to the moment.
-}
toMoment : TimeZone -> Time -> Date -> Moment
toMoment zone time date =
    Moment.fromMsSinceEpoch 0


{-| Convert to the Julian Day Number.
-}
toJDN : Date -> Int
toJDN (JDN jdn) =
    jdn


{-| What day of the week is it?
-}
toWeekday : Date -> Weekday
toWeekday (JDN jdn) =
    case modBy 7 jdn + 1 of
        1 ->
            Monday

        2 ->
            Tuesday

        3 ->
            Wednesday

        4 ->
            Thursday

        5 ->
            Friday

        6 ->
            Saterday

        _ ->
            Sunday


{-| Convert the weekday to a number representing the weekday.
1 is Monday, 7 is Sunday.
-}
toWeekdayNumber : Weekday -> Int
toWeekdayNumber weekday =
    case weekday of
        Monday ->
            1

        Tuesday ->
            2

        Wednesday ->
            3

        Thursday ->
            4

        Friday ->
            5

        Saterday ->
            6

        Sunday ->
            7


toNoon : TimeZone -> Date -> Moment
toNoon zone (JDN jdn) =
    let
        noonInUtc =
            Moment.fromMsSinceEpoch <| (jdn - 2440588) * 86400000 + 43200000
    in
    Moment.intoPastForZone zone noonInUtc


{-| Create the date that corresponds to the Julian Day Number.
Can be used by a calendar to create the correct date.
-}
fromJDN : Int -> Date
fromJDN jdn =
    JDN jdn



---- Move dates ----


{-| Move the date a number of days into the future.
-}
intoFuture : Duration -> Date -> Date
intoFuture (Duration numberOfDays) (JDN date) =
    JDN (date + numberOfDays)


{-| Move the date a number of days into the past.
-}
intoPast : Duration -> Date -> Date
intoPast (Duration numberOfDays) date =
    intoFuture (Duration -numberOfDays) date


{-| Move the date into the future until the next day that is a specific weekday.
If date is the weekday, it will move a week into the future.
-}
next : Weekday -> Date -> Date
next weekday date =
    let
        target =
            toWeekdayNumber weekday

        current =
            (toWeekday >> toWeekdayNumber) date

        diff =
            modBy 7 <| target - current

        toMove =
            if diff == 0 then
                weeks 1

            else
                days diff
    in
    intoFuture toMove date


{-| Move the date into the past until the previous day that is a specific weekday.
If date is the weekday, it will move a week into the past.
-}
last : Weekday -> Date -> Date
last weekday date =
    let
        target =
            toWeekdayNumber weekday

        current =
            (toWeekday >> toWeekdayNumber) date

        diff =
            modBy 7 <| current - target

        toMove =
            if diff == 0 then
                weeks 1

            else
                days diff
    in
    intoPast toMove date



---- Duration ----


{-| Duration represents a laps of time. It is represented in the date and time model,
because we are thinking about actual elaps of specific days and/or weeks.
-}
type Duration
    = Duration Int


{-| A duration of a number of days.
-}
days : Int -> Duration
days noOfDays =
    Duration noOfDays


{-| A duration of a number of weeks.
-}
weeks : Int -> Duration
weeks noOfWeeks =
    Duration <| noOfWeeks * 7


{-| Combine two durations.

It has an odd signiture to be able to efficiently use it using the pipe (|>) operator.
Example:

    weeks 2
        |> and days 15
    --> days 29

-}
and : (Int -> Duration) -> Int -> Duration -> Duration
and fct value (Duration duration) =
    let
        (Duration toAdd) =
            fct value
    in
    Duration (duration + toAdd)
