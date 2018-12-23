module Chrono.Date exposing
    ( Date
    , fromJDN
    , fromMoment
    , toJDN
    , toMoment
    , toNoon
    , toWeekday
    )

import Chrono.Moment as Moment exposing (Moment)
import Chrono.Time exposing (Time)
import Task exposing (Task)
import Time


{-| A date is an abstract understanding of a period of time.
It is independent of a time zone.

The internal representation of the Date is the Julian Day Number.
This is the number of days since the Julian day number 0, which is
Monday, January 1, 4713 BC, proleptic Julian calendar or
November 24, 4714 BC, in the proleptic Gregorian calendar.

-}
type Date
    = Date Int


{-| Get the date at the moment when this task is run and in the time zone
where this task is run.
-}
today : Task x Date
today =
    Task.map2 fromMoment (Task.map Chrono.Time.zoneWithSameOffset Time.here) <|
        Task.map (Moment.fromMsSinceEpoch << Time.posixToMillis) Time.now


fromMoment : Chrono.Time.Zone -> Moment -> Date
fromMoment zone moment =
    let
        shiftedPosix =
            Moment.toMsAfterEpoch <|
                Chrono.Time.moveIntoFutureForZone zone moment

        positiveJdn =
            shiftedPosix // 86400000 + 2440588

        jdn =
            if shiftedPosix < 0 then
                positiveJdn - 1

            else
                positiveJdn
    in
    Date jdn


andTimeFromMoment : Chrono.Time.Zone -> Time -> Moment -> { date : Date, time : Time }
andTimeFromMoment zone time moment =
    { date = fromMoment zone moment
    , time = Chrono.Time.fromMoment zone moment
    }


toMoment : Time.Zone -> Time -> Date -> Moment
toMoment zone time date =
    Moment.fromMsSinceEpoch 0


{-| Convert to the Julian Day Number.
-}
toJDN : Date -> Int
toJDN (Date jdn) =
    jdn


{-| What day of the week is it?
-}
toWeekday : Date -> Time.Weekday
toWeekday (Date jdn) =
    case modBy 7 jdn + 1 of
        1 ->
            Time.Mon

        2 ->
            Time.Tue

        3 ->
            Time.Wed

        4 ->
            Time.Thu

        5 ->
            Time.Fri

        6 ->
            Time.Sat

        _ ->
            Time.Sun


toNoon : Chrono.Time.Zone -> Date -> Moment
toNoon zone (Date jdn) =
    let
        noonInUtc =
            Moment.fromMsSinceEpoch <| (jdn - 2440588) * 86400000 + 43200000
    in
    Chrono.Time.moveIntoPastForZone zone noonInUtc


{-| Create the date that corresponds to the Julian Day Number.
Can be used by a calendar to create the correct date.
-}
fromJDN : Int -> Date
fromJDN jdn =
    Date jdn


{-| Move the date a number of days into the future.
Negative numbers will move to the past.
-}
move : Int -> Date -> Date
move numberOfDays (Date date) =
    Date (date + numberOfDays)


toMonthNumber : Time.Month -> Int
toMonthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
