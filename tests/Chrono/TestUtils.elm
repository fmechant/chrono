module Chrono.TestUtils exposing
    ( fuzzDate
    , fuzzDateDuration
    , fuzzDuration
    , fuzzMoment
    , fuzzMonth
    , fuzzThursday
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


fuzzMoment : Fuzz.Fuzzer Moment
fuzzMoment =
    Fuzz.map Moment.fromMsSinceEpoch <| Fuzz.intRange 0 Random.maxInt


fuzzDuration : Fuzz.Fuzzer Moment.Duration
fuzzDuration =
    Fuzz.map Moment.milliseconds <| Fuzz.map abs Fuzz.int


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
