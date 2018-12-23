module Chrono.Moment exposing (Moment, fromMsSinceEpoch, now, toMsAfterEpoch)

import Task exposing (Task)
import Time


{-| A specific moment in time.
-}
type Moment
    = Moment Int


now : Task x Moment
now =
    Task.map (fromMsSinceEpoch << Time.posixToMillis) Time.now


fromMsSinceEpoch : Int -> Moment
fromMsSinceEpoch ms =
    Moment ms


toMsAfterEpoch : Moment -> Int
toMsAfterEpoch (Moment ms) =
    ms
