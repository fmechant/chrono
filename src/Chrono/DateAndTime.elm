module Chrono.DateAndTime exposing
    ( DateAndTime
    , fromMoment
    , now
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


{-| Get the date and time at the moment when this task is run and in the time zone
where this task is run.
-}
now : Task x DateAndTime
now =
    Task.map2 fromMoment (Task.map TimeZone.withSameOffset CoreTime.here) <|
        Task.map (Moment.fromMsSinceEpoch << CoreTime.posixToMillis) CoreTime.now
