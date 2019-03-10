module Chrono.TimeZone exposing
    ( TimeZone
    , customZone
    , here
    , relevantOffset
    , utc
    , withSameOffset
    )

{-| The TimeZone module represents the time zone.

We defined a different time zone type that defined in the standard Time library,
because we need access to the offset.

-}

import Task exposing (Task)
import Time as CoreTime


type TimeZone
    = TimeZone Int (List Era)


type alias Era =
    { start : Int
    , offset : Int
    }


utc : TimeZone
utc =
    TimeZone 0 []


customZone : Int -> List { start : Int, offset : Int } -> TimeZone
customZone =
    TimeZone


withSameOffset : CoreTime.Zone -> TimeZone
withSameOffset zone =
    let
        day =
            CoreTime.toDay zone (CoreTime.millisToPosix 0)

        hour =
            CoreTime.toHour zone (CoreTime.millisToPosix 0)

        minute =
            CoreTime.toMinute zone (CoreTime.millisToPosix 0)

        offset =
            if day == 1 then
                hour * 60 + minute

            else
                (hour - 24) * 60 + minute
    in
    TimeZone offset []


relevantOffset : TimeZone -> Int -> Int
relevantOffset (TimeZone defaultOffset eras) ms =
    minutesInMs <|
        case List.head (List.filter (\era -> minutesInMs era.start < ms) eras) of
            Just era ->
                era.offset

            Nothing ->
                defaultOffset


here : Task x TimeZone
here =
    Task.map withSameOffset CoreTime.here


minutesInMs : Int -> Int
minutesInMs minutes =
    minutes * 60000
