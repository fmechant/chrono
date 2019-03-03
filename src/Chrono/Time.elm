module Chrono.Time exposing
    ( Time
    , fromMoment
    , toMsSinceNoon
    )

{-| A specific time of the day.
-}

import Chrono.Moment as Moment exposing (Moment)
import Chrono.TimeZone as TimeZone exposing (TimeZone)
import List
import Task exposing (Task)
import Time


{-| A specific time of the day.

The internal representation is the number of milliseconds from noon (12:00).

-}
type Time
    = Time Int


fromMoment : TimeZone -> Moment -> Time
fromMoment zone moment =
    Time <|
        modBy 86400000 (Moment.toMsAfterEpoch <| Moment.intoFutureForZone zone moment)
            - 43200000


toMsSinceNoon : Time -> Int
toMsSinceNoon (Time ms) =
    ms
