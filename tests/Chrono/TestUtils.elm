module Chrono.TestUtils exposing
    ( daylightSavingsTimeZone
    , fuzzDate
    , fuzzDateDuration
    , fuzzDuration
    , fuzzMoment
    , fuzzMonth
    , fuzzNonZeroDuration
    , fuzzThursday
    , fuzzYear
    )

import Chrono.Date as Date exposing (Date)
import Chrono.GregorianCalendar as Cal
import Chrono.Moment as Moment exposing (Moment)
import Chrono.TimeZone as TimeZone exposing (TimeZone)
import Fuzz
import Random


fuzzDate : Fuzz.Fuzzer Date
fuzzDate =
    Fuzz.map Date.fromJDN <| Fuzz.intRange 2415021 3415021


fuzzMoment : Fuzz.Fuzzer Moment
fuzzMoment =
    Fuzz.map Moment.fromMsSinceEpoch <| Fuzz.intRange 0 Random.maxInt


fuzzDuration : Fuzz.Fuzzer Moment.Duration
fuzzDuration =
    Fuzz.map Moment.milliseconds <| Fuzz.map abs Fuzz.int


fuzzNonZeroDuration : Fuzz.Fuzzer Moment.Duration
fuzzNonZeroDuration =
    Fuzz.map Moment.milliseconds <| Fuzz.map ((+) 1) <| Fuzz.map abs Fuzz.int


fuzzDateDuration : Fuzz.Fuzzer Date.Duration
fuzzDateDuration =
    Fuzz.map Date.days (Fuzz.intRange 0 Random.maxInt)


fuzzThursday : Fuzz.Fuzzer Date
fuzzThursday =
    Fuzz.map (\i -> Date.fromJDN ((i // 7) * 7 + 3)) Fuzz.int


fuzzMonth : Fuzz.Fuzzer Cal.Month
fuzzMonth =
    Fuzz.map Cal.fromMonthNumber (Fuzz.intRange 1 12)


fuzzYear : Fuzz.Fuzzer Int
fuzzYear =
    Fuzz.intRange 1900 2100


{-| TimeZone that switches from +01:00 to +02:00 at the given moment.
-}
daylightSavingsTimeZone : Moment -> TimeZone
daylightSavingsTimeZone moment =
    let
        start =
            round <| (toFloat <| Moment.toMsAfterEpoch moment) / 60000
    in
    TimeZone.customZone 60 [ { start = start, offset = 120 } ]
