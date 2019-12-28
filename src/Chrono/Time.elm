module Chrono.Time exposing
    ( Hour
    , Meridiem(..)
    , Time
    , am
    , chronologicalComparison
    , fromMoment
    , h24
    , m
    , midnight
    , ms
    , noon
    , pm
    , to12Hours
    , toMsSinceNoon
    , view
    )

{-| A specific time of the day, like 14:00:00 or 6:00 PM.

You can get the time from a moment, using a time zone.
You can create a time using 12 hours or 24 hours notation.

It also contains a function to view the time and to chronologically compare times.


# What about time travel?

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

    import Chrono.Date as Date
    import Chrono.DateAndTime as DateAndTime
    import Chrono.Moment as Moment
    import Chrono.TimeZone as TimeZone

    let
        switchMoment =
            Moment.fromMsSinceEpoch 1553994000000
        zone =
            TimeZone.customZone 60 [ { start = 25899900, offset = 120 } ]
        date =
            Date.fromMoment zone switchMoment
        oneAClock = h24 1 |> m 0
    in
    date
        |> DateAndTime.withTime oneAClock
        |> Debug.log "01:00"
        |> DateAndTime.toMoment zone
        |> Moment.intoFuture (Moment.hours 2)
        |> DateAndTime.fromMoment zone
        |> .time
        --> h24 4 |> m 0

Using date:

    import Chrono.Date as Date
    import Chrono.DateAndTime as DateAndTime
    import Chrono.Moment as Moment
    import Chrono.TimeZone as TimeZone

    let
        switchMoment =
            Moment.fromMsSinceEpoch 1553994000000
        zone =
            TimeZone.customZone 60 [ { start = 25899900, offset = 120 } ]
        date =
            Date.fromMoment zone switchMoment
        threeAClock = h24 3 |> m 0
    in
    date
        |> DateAndTime.withTime threeAClock
        |> .time
        --> h24 3 |> m 0

-}

import Chrono.Moment as Moment exposing (Moment)
import Chrono.TimeZone as TimeZone exposing (TimeZone)
import List
import Task exposing (Task)


{-| A specific time of the day.

The internal representation is the number of milliseconds from noon (12:00).

-}
type Time
    = Time Int


{-| An incomplete time type that only contains the hour of the time.
Use m to complete it to a Time type.

Use am, pm or h24 to create it.

-}
type Hour
    = Hour Int


{-| Get the time for this moment in this time zone.
-}
fromMoment : TimeZone -> Moment -> Time
fromMoment zone moment =
    Time <|
        modBy 86400000 (Moment.toMsAfterEpoch <| Moment.intoFutureForZone zone moment)
            - 43200000


{-| Get the milliseconds since noon.
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
        |> view
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
        |> view
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
        |> view
    --> {hour24 = 19, minute = 15, second = 0, millisecond = 0}

-}
h24 : Int -> Hour
h24 value =
    Hour <| (clamp 0 23 value - 12)


{-| Add minutes to the hour to make it a Time.

Example:

    am 7 |> m 15 -- 7:15:00,000
        |> view
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
-}
midnight : Time
midnight =
    Time (-12 * 3600000)


{-| Compare two times chronologically. Typically used with `List.sortWith`.

    import List

    let
        base = noon
        later = h24 14 |> m 0
        earlier = h24 3 |> m 30
    in
    [earlier, base, later] == List.sortWith chronologicalComparison [later, earlier, base]
    --> True

-}
chronologicalComparison : Time -> Time -> Order
chronologicalComparison (Time t) (Time u) =
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

Instead of using formatting strings, we prefer here to make it easy to make your
own view functions.

Example:

    let
        myTimeView : Time -> String
        myTimeView =
            let
                toStr = String.padLeft 2 '0' << String.fromInt
                format t =
                    toStr t.hour24 ++ ":" ++ toStr t.minute ++ ":" ++ toStr t.second
            in
            view >> format
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
                {hour24, minute, second, millisecond} = view t
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
view : Time -> { hour24 : Int, minute : Int, second : Int, millisecond : Int }
view (Time time) =
    let
        noonTo12 =
            time + 43200000

        ( wholeHours, withoutHours ) =
            substractWhole noonTo12 3600000

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
