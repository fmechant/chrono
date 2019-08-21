module Chrono.TestUtils exposing (fuzzCalDuration, fuzzDate, fuzzDateDuration, fuzzDuration, fuzzMoment, fuzzThursday)

import Chrono.Date as Date exposing (Date)
import Chrono.GregorianCalendar as Cal
import Chrono.Moment as Moment exposing (Moment)
import Fuzz
import Random


fuzzDate : Fuzz.Fuzzer Date
fuzzDate =
    Fuzz.map Date.fromJDN <| Fuzz.intRange 2415021 3415021


fuzzCalDuration : Fuzz.Fuzzer Cal.Moves
fuzzCalDuration =
    Fuzz.oneOf
        [ Fuzz.map Cal.days <| Fuzz.intRange 0 Random.maxInt
        , Fuzz.map2 Cal.months (Fuzz.intRange 0 1000) (Fuzz.constant Cal.stayInSameMonth)
        , Fuzz.map2 Cal.years (Fuzz.intRange 0 40) (Fuzz.constant Cal.stayInSameMonth)
        ]


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
