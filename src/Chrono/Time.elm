module Chrono.Time exposing
    ( Time
    , Zone
    , customZone
    , fromMoment
    , here
    , moveIntoFutureForZone
    , moveIntoPastForZone
    , toMsSinceNoon
    , utc
    , zoneWithSameOffset
    )

{-| A specific time of the day.
-}

import Chrono.Moment as Moment exposing (Moment)
import List
import Task exposing (Task)
import Time


{-| A specific time of the day.

The internal representation is the number of milliseconds from noon (12:00).

-}
type Time
    = Time Int


type Zone
    = Zone Int (List Era)


type alias Era =
    { start : Int
    , offset : Int
    }


here : Task x Zone
here =
    Task.map zoneWithSameOffset Time.here


zoneWithSameOffset : Time.Zone -> Zone
zoneWithSameOffset zone =
    let
        day =
            Time.toDay zone (Time.millisToPosix 0)

        hour =
            Time.toHour zone (Time.millisToPosix 0)

        minute =
            Time.toMinute zone (Time.millisToPosix 0)

        offset =
            if day == 1 then
                hour * 60 + minute

            else
                (hour - 24) * 60 + minute
    in
    Zone offset []


fromMoment : Zone -> Moment -> Time
fromMoment zone moment =
    Time <|
        modBy 86400000 (Moment.toMsAfterEpoch <| moveIntoFutureForZone zone moment)
            - 43200000


toMsSinceNoon : Time -> Int
toMsSinceNoon (Time ms) =
    ms


utc : Zone
utc =
    Zone 0 []


customZone : Int -> List { start : Int, offset : Int } -> Zone
customZone =
    Zone


moveIntoFutureForZone : Zone -> Moment -> Moment
moveIntoFutureForZone zone moment =
    let
        ms =
            Moment.toMsAfterEpoch moment
    in
    Moment.fromMsSinceEpoch (ms + relevantOffset zone ms)


moveIntoPastForZone : Zone -> Moment -> Moment
moveIntoPastForZone zone moment =
    let
        ms =
            Moment.toMsAfterEpoch moment
    in
    Moment.fromMsSinceEpoch (ms - relevantOffset zone ms)


relevantOffset : Zone -> Int -> Int
relevantOffset (Zone defaultOffset eras) ms =
    minutesInMs <|
        case List.head (List.filter (\era -> minutesInMs era.start < ms) eras) of
            Just era ->
                era.offset

            Nothing ->
                defaultOffset


minutesInMs : Int -> Int
minutesInMs minutes =
    minutes * 60000
