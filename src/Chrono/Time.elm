module Chrono.Time exposing
    ( Hour
    , Time
    , am
    , fromMoment
    , h24
    , m
    , midnight
    , ms
    , noon
    , pm
    , toMsSinceNoon
    )

{-| A specific time of the day.
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
-}
type Hour
    = Hour Int


fromMoment : TimeZone -> Moment -> Time
fromMoment zone moment =
    Time <|
        modBy 86400000 (Moment.toMsAfterEpoch <| Moment.intoFutureForZone zone moment)
            - 43200000


toMsSinceNoon : Time -> Int
toMsSinceNoon (Time milliseconds) =
    milliseconds


{-| Construct the time using an hour AM.
It needs m to make it a complete Time.

Values below 1 are considered as 1, values above 12 are considered as 12.

12:00 AM is midnight.

Example:

    am 7 |> m 15 -- 7:15:00,000

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

-}
h24 : Int -> Hour
h24 value =
    Hour <| (clamp 0 23 value - 12)


{-| Add minutes to the hour to make it a Time.

Example:

    am 7 |> m 15 -- 7:15:00,000

-}
m : Int -> Hour -> Time
m value (Hour hour) =
    Time <| (hour * 60 + clamp 0 59 value) * 60000


{-| Takes the hours and minutes of the time and adds the milliseconds.
The previous value of milliseconds is overridden.
So:

    (h24 2 |> m 5 |> ms 100) |> ms 45 --> h24 2 |> m 5 |> ms 45

We opted for this solution so it is not necessary to use ms, if you do not need it.
Most problems do not need milliseconds.

-}
ms : Time -> Int -> Time
ms (Time time) value =
    Time <| (time // 1000) * 1000 + clamp 0 1000 value


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
