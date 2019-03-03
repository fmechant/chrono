module Chrono.DateAndTime exposing (DateAndTime)

import Chrono.Date as Date exposing (Date)
import Chrono.Moment as Moment exposing (Moment)
import Chrono.Time as Time exposing (Time)
import Chrono.TimeZone as TimeZone exposing (TimeZone)


type alias DateAndTime =
    { date : Date
    , time : Time
    }


{-| -}
fromMoment : TimeZone -> Time -> Moment -> DateAndTime
fromMoment zone time moment =
    { date = Date.fromMoment zone moment
    , time = Time.fromMoment zone moment
    }
