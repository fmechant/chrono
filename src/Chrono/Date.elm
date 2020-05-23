module Chrono.Date exposing
    ( Date, now, chronologicalDateComparison
    , DateAndTime, withTime, toNoon, toDateAndTime, toMoment
    , Weekday(..), toWeekday, toWeekdayNumber
    , intoFuture, intoPast, next, last, collect
    , Duration, days, weeks, and, elapsed, durationView
    , fromJDN, toJDN
    , Time, Hour, am, pm, h24, m, ms, noon, midnight, chronologicalTimeComparison
    , timeView, to12Hours, Meridiem(..)
    , fromMsSinceNoon, toMsSinceNoon
    , TimeZone(..), utc, here
    , customZone, Period, Mapping
    )

{-| Module for working with dates, time, time zones, durations.


# Date

A date is an abstract understanding of a period of time.
What [elm/time][coretime] calls _Human Time_ and what [Abseil][abseil] calls
_Civil Time_. A specific date is an abstract concept that is not directly
linked to a moment in time.
It is independent of a time zone or a calendar.

For `Date`, days and weeks make sense. If you are looking for months or years,
look into [GregorianCalendar][./GregorianCalendar].

@docs Date, now, chronologicalDateComparison

[coretime]: https://package.elm-lang.org/packages/elm/time/latest
[abseil]: https://abseil.io/docs/cpp/guides/time


## Date and Time, combined

@docs DateAndTime, withTime, toNoon, toDateAndTime, toMoment


## Weekday

@docs Weekday, toWeekday, toWeekdayNumber


## Time Travel

You can move dates days or weeks. If you want more time travel, look into
[GregorianCalendar][./GregorianCalendar].

@docs intoFuture, intoPast, next, last, collect


## Duration

@docs Duration, days, weeks, and, elapsed, durationView


## Date import/export

@docs fromJDN, toJDN


# Time

A specific time of the day, like 14:00:00 or 6:00 PM.

You can get the time from a moment, using a time zone.
You can create a time using 12 hours or 24 hours notation.

It also contains a function to timeView the time and to chronologically compare times.

@docs Time, Hour, am, pm, h24, m, ms, noon, midnight, chronologicalTimeComparison


## Viewing Time

@docs timeView, to12Hours, Meridiem


## Time import/export

@docs fromMsSinceNoon, toMsSinceNoon


## What about time travel?

There are no functions to travel in time, because of the potential errors involved.
When we add two hours to 01:00, we expect 3:00, but that is not always what we want,
because of daylight savings.

The only way to travel through time is using either moments or dates. If you want
to have the time two hours later, you use Moment. If you want the hour to be two higher,
you use Date.

Example:
Suppose you are at 2019-03-21 01:00:00 GMT+01:00 DST, right before the daylight-savings
switch. What does going 2 hours in the future mean?

Using moment (switch of daylight-savings time on 2019-03-21 03:00:00 GMT+02:00 DST):

    import Chrono.Moment as Moment

    let
        switchMoment =
            -- 2019-03-21 03:00:00 GMT+02:00 DST
            Moment.fromMsSinceEpoch 1553994000000
        switchDate =
            fromJDN 2458574
        zone =
            customZone { moment = Moment.fromMsSinceEpoch 0, dateTime = { date = fromJDN 2440588, time = h24 1 |> m 0 } }
                [ { start = { moment = switchMoment, dateTime = { date = switchDate, time = h24 3 |> m 0 } } } ]
        oneAClock =
            h24 1 |> m 0
    in
    switchDate
        |> withTime oneAClock
        |> toMoment zone
        |> Moment.intoFuture (Moment.hours 2)
        |> toDateAndTime zone
        |> .time
        --> h24 4 |> m 0

Using date:

    import Chrono.Moment as Moment

    let
        switchMoment =
            -- 2019-03-21 03:00:00 GMT+02:00 DST
            Moment.fromMsSinceEpoch 1553994000000
        switchDate =
            fromJDN 2458564
        zone =
            customZone { moment = Moment.fromMsSinceEpoch 0, dateTime = { date = fromJDN 2440588, time = h24 1 |> m 0 } }
                [ { start = { moment = switchMoment, dateTime = { date = switchDate, time = h24 3 |> m 0 } } } ]
        threeAClock =
            h24 3 |> m 0
    in
    switchDate
        |> withTime threeAClock
        |> .time
        --> h24 3 |> m 0


# TimeZone

You use a `TimeZone` to map between a `Moment` and a `DateAndTime`.

@docs TimeZone, utc, here


## Creating you own Time Zones

@docs customZone, Period, Mapping

-}

import Chrono.Moment as Moment exposing (Direction(..), Moment)
import Task exposing (Task)
import Time as CoreTime



---- Date ----


{-| A date is an abstract understanding of a period of time.

The internal representation of the Date is the Julian Day Number.
This is the number of days since the Julian day number 0, which is
Monday, January 1, 4713 BC, proleptic Julian calendar or
November 24, 4714 BC, in the proleptic Gregorian calendar.

-}
type Date
    = JDN Int


{-| Create the date that corresponds to the Julian Day Number.
Can be used by a calendar to create the correct date.
-}
fromJDN : Int -> Date
fromJDN =
    JDN


{-| Convert to the Julian Day Number.
Avoid using this for calculations, let this library do the hard work for you.
-}
toJDN : Date -> Int
toJDN (JDN jdn) =
    jdn


{-| Maps a moment to a date/time, when the moment is inside a period with a
specific mapping.

It is used internally to find the date in a period.

It assumes the moment is inside the period, because it depends on a
one-to-one relationship (bijection) between moments and date/time.
Inside a period, this is correct.

-}
map : Mapping -> Moment -> DateAndTime
map start moment =
    let
        ( Moment.TimeLapse sinceStart, direction ) =
            Moment.elapsed start.moment moment

        ( daysInFuture, msInFuture ) =
            substractWhole sinceStart twentyFourHoursInMs

        (Time startTime) =
            start.dateTime.time
    in
    case direction of
        Moment.IntoTheFuture ->
            let
                startSecondsTillNextDay =
                    twelveHoursInMs - startTime
            in
            case msInFuture > startSecondsTillNextDay of
                True ->
                    intoFuture (days (daysInFuture + 1)) start.dateTime.date
                        |> withTime (fromMsSinceNoon ((startTime + msInFuture) - twentyFourHoursInMs))

                False ->
                    intoFuture (days daysInFuture) start.dateTime.date
                        |> withTime (fromMsSinceNoon (startTime + msInFuture))

        Moment.IntoThePast ->
            let
                startSecondsTillPreviousDay =
                    startTime + twelveHoursInMs
            in
            case msInFuture > startSecondsTillPreviousDay of
                True ->
                    intoPast (days (daysInFuture + 1)) start.dateTime.date
                        |> withTime (fromMsSinceNoon (twentyFourHoursInMs + startTime - msInFuture))

                False ->
                    intoPast (days daysInFuture) start.dateTime.date
                        |> withTime (fromMsSinceNoon (startTime - msInFuture))


{-| The epoch date is the date that started the moment epoch.
-}
epochDate : Date
epochDate =
    JDN 2440588


{-| Compare two dates chronologically. Typically used with `List.sortWith`.

Example:

    import List

    let
        base = fromJDN 0
        later = intoFuture (days 5) base
        earlier = intoPast (days 20) base
    in
    [earlier, base, later] == List.sortWith chronologicalDateComparison [later, earlier, base]
    --> True

-}
chronologicalDateComparison : Date -> Date -> Order
chronologicalDateComparison (JDN d) (JDN e) =
    compare d e



---- Weekday


{-| A weekday.
-}
type Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday


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
            Saturday

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

        Saturday ->
            6

        Sunday ->
            7



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


{-| Move the date into the future until that day is a specific weekday.
If the date is the weekday, it will move a week into the future.
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


{-| Move the date into the past until that day is a specific weekday.
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


{-| Collect a list of dates, starting from a date and making a number of jumps.
Combine this with GregorianCalendar.travel for more interesting collects.

Example: Collect the next three Sundays after November 24, 4714 BC, in the proleptic Gregorian calendar

    let
        base = fromJDN 0
    in
    collect 3 (next Sunday) base
    --> [fromJDN 6, fromJDN 13, fromJDN 20]

-}
collect : Int -> (Date -> Date) -> Date -> List Date
collect length toNext date =
    let
        collector =
            \_ ( acc, currentDate ) ->
                let
                    nextDate =
                        toNext currentDate
                in
                ( acc ++ [ nextDate ], nextDate )
    in
    List.repeat length 0
        |> List.foldr collector ( [], date )
        |> Tuple.first



---- Duration ----


{-| Duration represents a lapse of time. It is represented in the date and time model,
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


{-| Show the duration split up in weeks and days.

Typically used to create your own specific view.
Example:

    viewDaysAndWeeksShort duration =
        let
            daysAndWeeks =
                viewDuration duration
        in
        case ( daysAndWeeks.weeks, daysAndWeeks.days ) of
            ( 0, 0 ) ->
                "0"

            ( 0, noDays ) ->
                String.fromInt noDays ++ "d"

            ( noWeeks, noDays ) ->
                String.fromInt noWeeks ++ "w " ++ String.fromInt noDays ++ "d"

-}
durationView : Duration -> { days : Int, weeks : Int }
durationView (Duration totalDays) =
    let
        numberOfWeeks =
            totalDays // 7
    in
    { days = totalDays - numberOfWeeks * 7, weeks = numberOfWeeks }


{-| How many days have elapsed between the dates.

The result is a duration, with the indication whether one date is in the future
or in the past regarding to the other date.

-}
elapsed : Date -> Date -> ( Duration, Direction )
elapsed (JDN from) (JDN to) =
    let
        diff =
            to - from

        dir =
            if diff < 0 then
                IntoThePast

            else
                IntoTheFuture
    in
    ( days <| abs <| diff, dir )



---- Time ----


{-| A specific time of the day, like 14:00:00 or 6:00 PM.
-}
type Time
    = Time Int -- The number of milliseconds from noon (12:00).


{-| An incomplete time type that only contains the hour of the time.
Use m to complete it to a Time type.

Use am, pm or h24 to create it.

-}
type Hour
    = Hour Int


{-| Create the time that corresponds to the number of milliseconds since noon.

Negative values give a time before noon, positieve in the afternoon.

-}
fromMsSinceNoon : Int -> Time
fromMsSinceNoon =
    Time


{-| Get the milliseconds since noon.

Avoid using this for calculations, let this library do the hard work for you.

-}
toMsSinceNoon : Time -> Int
toMsSinceNoon (Time milliseconds) =
    milliseconds


{-| Construct the time using an hour AM.
It needs m to make it a complete Time.

Values below 1 are considered as 1, values above 12 are considered as 12.

12:00 AM is midnight.

Example:

    am 7 |> m 15 -- 07:15:00,000
        |> timeView
    --> {hour24 = 7, minute = 15, second = 0, millisecond = 0}

-}
am : Int -> Hour
am =
    clamp 1 12 >> midnightNoonCorrection >> h24


{-| Construct the time using an hour PM.
It needs m to make it a complete Time.

Values below 1 are considered as 1, values above 12 are considered as 12.

12:00 PM is noon.

Example:

    pm 7 |> m 15 -- 19:15:00,000
        |> timeView
    --> {hour24 = 19, minute = 15, second = 0, millisecond = 0}

-}
pm : Int -> Hour
pm =
    clamp 1 12 >> midnightNoonCorrection >> (+) 12 >> h24


{-| The correction to make sure 12:00 AM is midnight and 12:00 PM is noon.
-}
midnightNoonCorrection : Int -> Int
midnightNoonCorrection int =
    case int of
        12 ->
            0

        _ ->
            int


{-| Construct the time using an hour on the 24-hours clock.
It needs m to make it a complete Time.

We avoid using 24 because of the confusion involved.
So, midnight is 0:00:00,000 and not 24:00:00,000.

Values below 0 are considered as 0, values above 23 are considered as 23.

Example:

    h24 19 |> m 15 -- 19:15:00,000
        |> timeView
    --> {hour24 = 19, minute = 15, second = 0, millisecond = 0}

-}
h24 : Int -> Hour
h24 value =
    Hour <| (clamp 0 23 value - 12)


{-| Add minutes to the hour to make it a Time.

Example:

    am 7 |> m 15 -- 7:15:00,000
        |> timeView
    --> {hour24 = 7, minute = 15, second = 0, millisecond = 0}

-}
m : Int -> Hour -> Time
m value (Hour hour) =
    Time <| (hour * 60 + clamp 0 59 value) * 60000


{-| Takes the hours and minutes of the time and adds the milliseconds.
The previous value of milliseconds is overridden.
So:

    (h24 13 |> m 5 |> ms 100) |> ms 45 --> h24 13 |> m 5 |> ms 45

We opted for this solution so it is not necessary to use ms, if you do not need it.
Most problems do not need milliseconds.

-}
ms : Int -> Time -> Time
ms value (Time time) =
    Time <| floor (toFloat time / 60000) * 60000 + clamp 0 60000 value


{-| Get the time for noon.
-}
noon : Time
noon =
    Time 0


{-| Get the time for midnight.

Returns 00:00:00. (We avoid 24:00:00 because of ambiguity.)

-}
midnight : Time
midnight =
    Time (-12 * oneHourInMs)


{-| Compare two times chronologically. Typically used with `List.sortWith`.

    import List

    let
        base = noon
        later = h24 14 |> m 0
        earlier = h24 3 |> m 30
    in
    [earlier, base, later] == List.sortWith chronologicalTimeComparison [later, earlier, base]
    --> True

-}
chronologicalTimeComparison : Time -> Time -> Order
chronologicalTimeComparison (Time t) (Time u) =
    compare t u



---- View


{-| AM or PM?
-}
type Meridiem
    = AM
    | PM


{-| Convert 24 hours to 12 hours (AM/PM).
Example:

    to12Hours 14 --> (2, PM)

-}
to12Hours : Int -> ( Int, Meridiem )
to12Hours hours24 =
    let
        hour12 =
            modBy 12 hours24

        hour12Corrected =
            case hour12 of
                0 ->
                    12

                _ ->
                    hour12

        meridiem =
            case hours24 >= 12 of
                True ->
                    PM

                False ->
                    AM
    in
    ( hour12Corrected, meridiem )


{-| View the time split up in hours, minutes, seconds and milliseconds.

Avoid using this for calculations, let this library do the hard work for you.

Instead of using formatting strings, we prefer here to make it easy to make your
own timeView functions.

Example:

    let
        myTimeView : Time -> String
        myTimeView =
            let
                toStr = String.padLeft 2 '0' << String.fromInt
                format t =
                    toStr t.hour24 ++ ":" ++ toStr t.minute ++ ":" ++ toStr t.second
            in
            timeView >> format
    in
    pm 4 |> m 9 |> ms 15000
        |> myTimeView
        --> "16:09:15"

Use to12Hours additionally if you want the hours in AM/PM format.
Example using meridiem:

    let
        myTimeView : Time -> String
        myTimeView t =
            let
                {hour24, minute, second, millisecond} = timeView t
                (hour, meridiem) = to12Hours hour24
                toStr = String.padLeft 2 '0' << String.fromInt
                meridiemView m =
                    case m of
                        AM -> "AM"
                        PM -> "PM"
            in
            String.fromInt hour ++ ":" ++ toStr minute ++ " " ++ meridiemView meridiem
    in
    pm 4 |> m 9 |> ms 15000
        |> myTimeView
        --> "4:09 PM"

-}
timeView : Time -> { hour24 : Int, minute : Int, second : Int, millisecond : Int }
timeView (Time time) =
    let
        noonTo12 =
            time + twelveHoursInMs

        ( wholeHours, withoutHours ) =
            substractWhole noonTo12 oneHourInMs

        ( wholeMinutes, withoutMinutes ) =
            substractWhole withoutHours 60000

        ( wholeSeconds, withoutSeconds ) =
            substractWhole withoutMinutes 1000
    in
    { hour24 = wholeHours
    , minute = wholeMinutes
    , second = wholeSeconds
    , millisecond = withoutSeconds
    }



---- TimeZone ----


{-| A time zone is defined by a default (bijective) mapping between moments and date/time and
periods when there is a different mapping.

New periods typically begin when there is a change in daylight savings time.

TODO: add handling of leap seconds here when moment is in TAI and no longer in Unix time.

-}
type TimeZone
    = TimeZone Mapping (List Period)


{-| A period is a part of a time zone where every moment maps one-to-one (bijectively) to a date/time.

To avoid possible conflicts in a time zone, only the start is explicitly defined.
The end is defined by the start moment of the next period, chronologicaly.

-}
type alias Period =
    { start : Mapping
    }


{-| Mapping defines a bijection between moments and date/times.

It enables to switch between moment and date/time in a period where they map one-to-one (bijectively).

-}
type alias Mapping =
    { moment : Moment
    , dateTime : DateAndTime
    }


{-| The time zone defined as coordinated universal time or UTC.

It maps 1 January 1970 to the epoch moment and never has a daylight savings time switch.

TODO: update time zone to take leap seconds into account

See <https://en.wikipedia.org/wiki/Coordinated_Universal_Time>

-}
utc : TimeZone
utc =
    let
        mapping =
            { moment = Moment.fromMsSinceEpoch 0
            , dateTime =
                { date = epochDate
                , time = midnight
                }
            }
    in
    TimeZone mapping []


{-| Create a custom zone with the mapping and periods.

Target audience is libraries that read the TimeZone information from the tz
database.

-}
customZone : Mapping -> List Period -> TimeZone
customZone defaultMapping periods =
    TimeZone defaultMapping periods


{-| A naïve implementation of transforming the elm/time Zone to this TimeZone.
It only works on zones without an era.

We need to find a way to deal with time zones that have eras.

-}
zoneWithSameOffset : CoreTime.Zone -> TimeZone
zoneWithSameOffset zone =
    let
        epoch =
            CoreTime.millisToPosix 0

        time =
            h24 (CoreTime.toHour zone epoch)
                |> m (CoreTime.toMinute zone epoch)
                |> ms (CoreTime.toSecond zone epoch * 1000 + CoreTime.toMillis zone epoch)

        date =
            case CoreTime.toDay zone epoch of
                1 ->
                    epochDate

                31 ->
                    intoPast (days 1) epochDate

                _ ->
                    -- should not happen, no time zone is more that 14 hours from utc.
                    epochDate

        mapping =
            Mapping (Moment.fromMsSinceEpoch 0) (withTime time date)
    in
    TimeZone mapping []


{-| Get the date and time at the moment when this task is run and in the time
zone where this task is run.
-}
now : Task x DateAndTime
now =
    Task.map2 toDateAndTime (Task.map zoneWithSameOffset CoreTime.here) <|
        Task.map (Moment.fromMsSinceEpoch << CoreTime.posixToMillis) CoreTime.now


here : Task x TimeZone
here =
    Task.map zoneWithSameOffset CoreTime.here



---- Conversion functions


{-| The relevant period in the time zone for the moment.
-}
relevantMappingForMoment : TimeZone -> Moment -> Mapping
relevantMappingForMoment (TimeZone mapping periods) moment =
    List.foldl
        (\period lastPeriod ->
            if Moment.earliest period.start.moment moment == period.start.moment then
                Just period

            else
                lastPeriod
        )
        Nothing
        periods
        |> Maybe.map .start
        |> Maybe.withDefault mapping


relevantMappingForDateAndTime : TimeZone -> DateAndTime -> Mapping
relevantMappingForDateAndTime (TimeZone mapping periods) { date, time } =
    List.foldl
        (\period lastPeriod ->
            case
                ( chronologicalDateComparison period.start.dateTime.date date
                , chronologicalTimeComparison period.start.dateTime.time time
                )
            of
                ( GT, _ ) ->
                    lastPeriod

                ( LT, _ ) ->
                    Just period

                ( EQ, GT ) ->
                    lastPeriod

                ( EQ, _ ) ->
                    Just period
        )
        Nothing
        periods
        |> Maybe.map .start
        |> Maybe.withDefault mapping


{-| Convert a moment to a date/time in a time zone.
-}
toDateAndTime : TimeZone -> Moment -> DateAndTime
toDateAndTime zone moment =
    let
        relevantMapping =
            relevantMappingForMoment zone moment
    in
    map relevantMapping moment


{-| Convert a date/time to a moment in a time zone.
-}
toMoment : TimeZone -> DateAndTime -> Moment
toMoment zone forDateTime =
    let
        { moment, dateTime } =
            relevantMappingForDateAndTime zone forDateTime

        (JDN startDate) =
            dateTime.date

        (Time startTime) =
            dateTime.time

        (JDN date) =
            forDateTime.date

        (Time time) =
            forDateTime.time

        -- We can do this, because there is a bijection between date/time and moment inside a period
        timeLapse =
            Moment.hours (24 * (date - startDate))
                |> Moment.and Moment.milliseconds (time - startTime)
    in
    Moment.intoFuture timeLapse moment


{-| Get the moment of noon in the given date. Noon being 12:00.
-}
toNoon : TimeZone -> Date -> Moment
toNoon zone date =
    toMoment zone (withTime noon date)



---- Date and Time ----


{-| A date and time.
-}
type alias DateAndTime =
    { date : Date
    , time : Time
    }


{-| Get the DateAndTime for the time and the date.
-}
withTime : Time -> Date -> DateAndTime
withTime time date =
    { date = date, time = time }



---- HELPER FUNCTIONS ----


{-| Subtract the whole part, when dividing by the factor, and return the whole part, and the remaining value.
-}
substractWhole : Int -> Int -> ( Int, Int )
substractWhole value factor =
    let
        whole =
            value // factor
    in
    ( whole, value - whole * factor )


twentyFourHoursInMs : Int
twentyFourHoursInMs =
    86400000


twelveHoursInMs : Int
twelveHoursInMs =
    43200000


oneHourInMs : Int
oneHourInMs =
    3600000
