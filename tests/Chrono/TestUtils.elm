module Chrono.TestUtils exposing
    ( epocDateTimeInUtc
    , epoch
    , epochDate
    , fuzzDate
    , fuzzDateAndTime
    , fuzzDateDuration
    , fuzzMoment
    , fuzzMonth
    , fuzzNonZeroTimeLapse
    , fuzzThursday
    , fuzzTime
    , fuzzTimeLapse
    , fuzzTimeZoneNoIntervals
    , fuzzTimeZoneWithInterval
    , fuzzYear
    )

import Chrono.Date as Date exposing (Date)
import Chrono.GregorianCalendar as Cal
import Chrono.Moment as Moment exposing (Moment)
import Fuzz
import Random


fuzzDate : Fuzz.Fuzzer Date
fuzzDate =
    Fuzz.map Date.fromJDN <| Fuzz.intRange 2415021 3415021


fuzzTime : Fuzz.Fuzzer Date.Time
fuzzTime =
    Fuzz.map Date.fromMsSinceNoon <| Fuzz.intRange -43200000 43199999


fuzzDateAndTime : Fuzz.Fuzzer Date.DateAndTime
fuzzDateAndTime =
    Fuzz.map2 Date.DateAndTime fuzzDate fuzzTime


fuzzMoment : Fuzz.Fuzzer Moment
fuzzMoment =
    Fuzz.map Moment.fromMsSinceEpoch <| Fuzz.intRange 0 Random.maxInt


fuzzTimeLapse : Fuzz.Fuzzer Moment.TimeLapse
fuzzTimeLapse =
    Fuzz.map Moment.milliseconds <| Fuzz.map abs Fuzz.int


fuzzNonZeroTimeLapse : Fuzz.Fuzzer Moment.TimeLapse
fuzzNonZeroTimeLapse =
    Fuzz.map Moment.milliseconds <| Fuzz.map ((+) 1) <| Fuzz.map abs Fuzz.int


fuzzDateDuration : Fuzz.Fuzzer Date.Duration
fuzzDateDuration =
    Fuzz.map Date.days (Fuzz.intRange 1 Random.maxInt)


fuzzThursday : Fuzz.Fuzzer Date
fuzzThursday =
    Fuzz.map (\i -> Date.fromJDN ((i // 7) * 7 + 3)) Fuzz.int


fuzzMonth : Fuzz.Fuzzer Cal.Month
fuzzMonth =
    Fuzz.map Cal.fromMonthNumber (Fuzz.intRange 1 12)


fuzzYear : Fuzz.Fuzzer Int
fuzzYear =
    Fuzz.intRange 1900 2100


fuzzTimeZoneNoIntervals : Fuzz.Fuzzer Date.TimeZone
fuzzTimeZoneNoIntervals =
    let
        ftz moment dateAndTime =
            Date.customZone { moment = moment, dateTime = dateAndTime } []
    in
    Fuzz.map2 ftz fuzzMoment fuzzDateAndTime


fuzzTimeZoneWithInterval : Fuzz.Fuzzer ( Date.TimeZone, Date.Mapping, Date.Interval )
fuzzTimeZoneWithInterval =
    let
        ftz moment dateAndTime timeLapse dateAndTimeInInterval =
            let
                interval =
                    { start =
                        { moment = Moment.intoFuture timeLapse moment
                        , dateTime = dateAndTimeInInterval
                        }
                    }

                mapping =
                    { moment = moment, dateTime = dateAndTime }
            in
            ( Date.customZone mapping [ interval ], mapping, interval )
    in
    Fuzz.map4 ftz fuzzMoment fuzzDateAndTime fuzzNonZeroTimeLapse fuzzDateAndTime



---- EPOCH RELATED ----


epoch : Moment
epoch =
    Moment.fromMsSinceEpoch 0


epochDate : Date
epochDate =
    Date.fromJDN 2440588


epocDateTimeInUtc : Date.DateAndTime
epocDateTimeInUtc =
    { date = epochDate, time = Date.h24 0 |> Date.m 0 }
