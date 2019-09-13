module Chrono.DateAndTime exposing
    ( DateAndTime
    , fromMoment
    , now
    , toMoment
    , withTime
    )

import Chrono.Date as Date exposing (Date)
import Chrono.Moment as Moment exposing (Moment)
import Chrono.Time as Time exposing (Time)
import Chrono.TimeZone as TimeZone exposing (TimeZone)
import Task exposing (Task)
import Time as CoreTime


{-| A date and time.
-}
type alias DateAndTime =
    { date : Date
    , time : Time
    }


{-| Get the DateAndTime that represents the moment in this time zone.
-}
fromMoment : TimeZone -> Moment -> DateAndTime
fromMoment zone moment =
    { date = Date.fromMoment zone moment
    , time = Time.fromMoment zone moment
    }


{-| Get the moment of this date and time in this time zone.
-}
toMoment : TimeZone -> DateAndTime -> Moment
toMoment zone { date, time } =
    Date.toNoon zone date
        |> Moment.toMsAfterEpoch
        |> (+) (Time.toMsSinceNoon time)
        |> Moment.fromMsSinceEpoch


{-| Get the DateAndTime for the time and the date.
-}
withTime : Time -> Date -> DateAndTime
withTime time date =
    { date = date, time = time }


{-| Get the date and time at the moment when this task is run and in the time zone
where this task is run.
-}
now : Task x DateAndTime
now =
    Task.map2 fromMoment (Task.map TimeZone.withSameOffset CoreTime.here) <|
        Task.map (Moment.fromMsSinceEpoch << CoreTime.posixToMillis) CoreTime.now
