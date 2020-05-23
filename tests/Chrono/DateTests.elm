module Chrono.DateTests exposing (all)

import Chrono.Date exposing (..)
import Chrono.Moment as Moment
import Chrono.TestUtils exposing (..)
import Expect
import Fuzz
import Test exposing (..)
import Time


all : Test
all =
    describe "Date tests"
        [ describe "toDateAndTime should use correct conversion to JDN."
            [ test "1 January 1970 is JDN 2,440,588." <|
                \() ->
                    firstJanuary1970
                        |> toJDN
                        |> Expect.equal 2440588
            , test "January 1, 2000 is JDN 2,451,545." <|
                \() ->
                    Moment.fromMsSinceEpoch 946684800000
                        |> toDateAndTime utc
                        |> .date
                        |> toJDN
                        |> Expect.equal 2451545
            , test "Epoch moment in GMT-5 is JDN 2,440,587" <|
                \() ->
                    epochMoment
                        |> toDateAndTime gmtMinus5
                        |> .date
                        |> toJDN
                        |> Expect.equal 2440587
            ]
        , describe "toWeekday"
            [ test "1 January 1970 is a Thursday." <|
                \() ->
                    firstJanuary1970
                        |> toWeekday
                        |> Expect.equal Thursday
            ]
        , describe "toNoon"
            [ test "toNoon on January 1, 1970 is 43200000 in utc." <|
                \() ->
                    firstJanuary1970
                        |> toNoon utc
                        |> Expect.equal (Moment.fromMsSinceEpoch 43200000)
            , test "toNoon on January 1, 1970 is 0 in GMT+12." <|
                \() ->
                    firstJanuary1970
                        |> toNoon gmtPlus12
                        |> Expect.equal (Moment.fromMsSinceEpoch 0)
            ]
        , describe "move"
            [ fuzz fuzzThursday "next to current weekday should move a week" <|
                \aThursday ->
                    aThursday
                        |> next Thursday
                        |> Expect.equal (intoFuture (weeks 1) aThursday)
            , fuzz fuzzThursday "next to day earlier in the week should move to next week" <|
                \aThursday ->
                    aThursday
                        |> next Wednesday
                        |> Expect.equal (intoFuture (days 6) aThursday)
            , fuzz fuzzThursday "next to day later this week should stay in this week" <|
                \aThursday ->
                    aThursday
                        |> next Saturday
                        |> Expect.equal (intoFuture (days 2) aThursday)
            , fuzz fuzzThursday "last to current weekday should move a week" <|
                \aThursday ->
                    aThursday
                        |> last Thursday
                        |> Expect.equal (intoPast (weeks 1) aThursday)
            , fuzz fuzzThursday "next to day earlier in the week should stay in this week" <|
                \aThursday ->
                    aThursday
                        |> last Wednesday
                        |> Expect.equal (intoPast (days 1) aThursday)
            , fuzz fuzzThursday "next to day later this week should move to the previous week" <|
                \aThursday ->
                    aThursday
                        |> last Saturday
                        |> Expect.equal (intoPast (days 5) aThursday)
            , fuzz2 fuzzDate Fuzz.int "moving weeks is the same are moving 7 times that in days." <|
                \aDate noWeeks ->
                    aDate
                        |> intoFuture (weeks noWeeks)
                        |> Expect.equal (intoFuture (days (noWeeks * 7)) aDate)
            , fuzz2 fuzzDate fuzzDateDuration "moving into future and then the same into past is staying here." <|
                \aDate aDuration ->
                    aDate
                        |> intoFuture aDuration
                        |> intoPast aDuration
                        |> Expect.equal aDate
            ]
        , describe "collect"
            [ test "5 days after 1 January is 2,3,4,5 and 6 January" <|
                \() ->
                    firstJanuary1970
                        |> collect 5 (intoFuture (days 1))
                        |> Expect.equalLists
                            [ intoFuture (days 1) firstJanuary1970
                            , intoFuture (days 2) firstJanuary1970
                            , intoFuture (days 3) firstJanuary1970
                            , intoFuture (days 4) firstJanuary1970
                            , intoFuture (days 5) firstJanuary1970
                            ]
            ]
        , describe "view duration"
            [ test "should show the correct number of days" <|
                \() ->
                    days 5
                        |> durationView
                        |> Expect.equal { days = 5, weeks = 0 }
            , test "should show 1 week for 7 days" <|
                \() ->
                    days 7
                        |> durationView
                        |> Expect.equal { days = 0, weeks = 1 }
            , fuzz2 (Fuzz.intRange 0 6) (Fuzz.intRange 0 10000) "should work out days and weeks" <|
                \someDays someWeeks ->
                    days someDays
                        |> and weeks someWeeks
                        |> durationView
                        |> Expect.equal { days = someDays, weeks = someWeeks }
            ]
        , describe "elapsed"
            [ fuzz2 fuzzDate fuzzDateDuration "should return a duration that gives the date if we move it into the future" <|
                \date duration ->
                    date
                        |> intoFuture duration
                        |> elapsed date
                        |> Expect.equal ( duration, Moment.IntoTheFuture )
            , fuzz2 fuzzDate fuzzDateDuration "should return a duration that gives the date if we move it into the past" <|
                \date duration ->
                    date
                        |> intoPast duration
                        |> elapsed date
                        |> Expect.equal ( duration, Moment.IntoThePast )
            ]
        , fuzz3 fuzzDate (Fuzz.intRange 1 10) (Fuzz.intRange 1 10) "chronological comparison" <|
            \date daysEarlier daysLater ->
                let
                    earlier =
                        intoPast (days daysEarlier) date

                    later =
                        intoFuture (days daysLater) date
                in
                [ later, earlier, date ]
                    |> List.sortWith chronologicalDateComparison
                    |> Expect.equal [ earlier, date, later ]
        , describe "Date and Time tests"
            [ describe "fromMoment"
                [ fuzz fuzzMoment "converting the date and time in utc and back to moment should be consistent." <|
                    \aMoment ->
                        aMoment
                            |> toDateAndTime utc
                            |> toMoment utc
                            |> Expect.equal aMoment
                , fuzz2 fuzzTimeZoneNoPeriods fuzzMoment "converting the date and time in any time zone and back to moment should be consistent." <|
                    \aZone aMoment ->
                        aMoment
                            |> toDateAndTime aZone
                            |> toMoment aZone
                            |> Expect.equal aMoment
                ]
            ]
        , describe "time"
            [ describe "when getting the time"
                [ test "the epoch moment should be -43,200,000 in utc." <|
                    \() ->
                        Moment.fromMsSinceEpoch 0
                            |> toDateAndTime utc
                            |> .time
                            |> toMsSinceNoon
                            |> Expect.equal -43200000
                , test "construct time using AM" <|
                    \() ->
                        am 7
                            |> m 15
                            |> toMsSinceNoon
                            |> Expect.equal (-5 * 3600000 + 15 * 60000)
                , test "construct time using PM" <|
                    \() ->
                        pm 7
                            |> m 15
                            |> toMsSinceNoon
                            |> Expect.equal (7 * 3600000 + 15 * 60000)
                , test "12 AM is midnight" <|
                    \() ->
                        am 12
                            |> m 0
                            |> Expect.equal midnight
                , test "12 PM is noon" <|
                    \() ->
                        pm 12
                            |> m 0
                            |> Expect.equal noon
                , fuzz (Fuzz.intRange 1 11) "am and h24 are interchangeable" <|
                    \hour ->
                        am hour
                            |> Expect.equal (h24 hour)
                , fuzz (Fuzz.intRange 1 11) "pm and h24 are interchangeable" <|
                    \hour ->
                        pm hour
                            |> Expect.equal (h24 (12 + hour))
                ]
            , describe "view"
                [ test "view time for noon" <|
                    \() ->
                        noon
                            |> timeView
                            |> Expect.equal
                                { hour24 = 12
                                , minute = 0
                                , second = 0
                                , millisecond = 0
                                }
                , test "to12Hours for noon" <|
                    \() ->
                        noon
                            |> timeView
                            |> .hour24
                            |> to12Hours
                            |> Expect.equal ( 12, PM )
                , test "to12Hours for mightnight" <|
                    \() ->
                        midnight
                            |> timeView
                            |> .hour24
                            |> to12Hours
                            |> Expect.equal ( 12, AM )
                ]
            , describe "milliseconds"
                [ test "milliseconds get overridden" <|
                    \() ->
                        (h24 13 |> m 5 |> ms 100)
                            |> ms 45
                            |> Expect.equal (h24 13 |> m 5 |> ms 45)
                , test "milliseconds get overridden for AM too" <|
                    \() ->
                        (h24 2 |> m 5 |> ms 100)
                            |> ms 45
                            |> Expect.equal (h24 2 |> m 5 |> ms 45)
                ]
            , daylightSavings
            ]
        , describe "Details about Time Zones"
            [ describe "toDateAndTime"
                [ fuzz fuzzTimeZoneWithPeriod "should return 'default' date/time before start of period" <|
                    \( aZone, defaultMapping, period ) ->
                        defaultMapping.moment
                            |> toDateAndTime aZone
                            |> Expect.equal defaultMapping.dateTime
                , fuzz fuzzTimeZoneWithPeriod "should return 'mapped' date/time at start of period" <|
                    \( aZone, defaultMapping, period ) ->
                        period.start.moment
                            |> toDateAndTime aZone
                            |> Expect.equal period.start.dateTime
                , fuzz fuzzTimeZoneWithPeriod "should return 'mapped' date/time after start of period" <|
                    \( aZone, defaultMapping, period ) ->
                        period.start.moment
                            |> Moment.intoFuture (Moment.hours 24)
                            -- 24h = 1d here because we stay in the period
                            |> toDateAndTime aZone
                            |> Expect.equal { date = intoFuture (days 1) period.start.dateTime.date, time = period.start.dateTime.time }
                ]
            ]
        ]


{-| Europe goes to DST on 2019-03-31 02:00:00 GMT+01:00 to 2019-03-31 03:00:00 GMT+02:00
-}
daylightSavings : Test
daylightSavings =
    let
        switchMoment =
            -- 2019-03-31 03:00:00 GMT+02:00 DST
            Moment.fromMsSinceEpoch 1553994000000

        switchDate =
            fromJDN 2458574

        zone =
            customZone { moment = epoch, dateTime = { date = epochDate, time = h24 1 |> m 0 } }
                [ { start = { moment = switchMoment, dateTime = { date = switchDate, time = h24 3 |> m 0 } } } ]
    in
    describe "zone with daylight saving period"
        [ test "should convert toMoment correctly, an hour before daylight saving" <|
            \() ->
                switchDate
                    |> withTime (h24 1 |> m 0)
                    |> toMoment zone
                    |> Expect.equal (Moment.intoPast (Moment.hours 1) switchMoment)
        , test "should convert toMoment correctly, an hour after daylight saving" <|
            \() ->
                switchDate
                    |> withTime (h24 4 |> m 0)
                    |> toMoment zone
                    |> Expect.equal (Moment.intoFuture (Moment.hours 1) switchMoment)
        , test "should convert toMoment correctly, a day before daylight saving" <|
            \() ->
                switchDate
                    |> intoPast (days 1)
                    |> withTime (h24 2 |> m 0)
                    |> toMoment zone
                    |> Expect.equal (Moment.intoPast (Moment.hours 24) switchMoment)
        , test "should convert toMoment correctly, a day after daylight saving" <|
            \() ->
                switchDate
                    |> intoFuture (days 1)
                    |> withTime (h24 3 |> m 0)
                    |> toMoment zone
                    |> Expect.equal (Moment.intoFuture (Moment.hours 24) switchMoment)
        , test "should return just before 2:00 before switch" <|
            \() ->
                switchMoment
                    |> Moment.intoPast (Moment.minutes 1)
                    |> toDateAndTime zone
                    |> Expect.equal { date = switchDate, time = h24 1 |> m 59 }
        , test "should return just after 3 after switch" <|
            \() ->
                switchMoment
                    |> Moment.intoFuture (Moment.minutes 1)
                    |> toDateAndTime zone
                    |> Expect.equal { date = switchDate, time = h24 3 |> m 1 }
        , test "should add difference when moving over the switch moment" <|
            \() ->
                let
                    before =
                        Moment.intoPast (Moment.hours 1) switchMoment
                in
                before
                    |> Moment.intoFuture (Moment.hours 2)
                    |> toDateAndTime zone
                    |> Expect.equal { date = switchDate, time = h24 4 |> m 0 }
        ]


{-| The epoch moment is Thurday, January 1, 1970.
-}
epochMoment : Moment.Moment
epochMoment =
    Moment.fromMsSinceEpoch 0


firstJanuary1970 : Date
firstJanuary1970 =
    fromJDN 2440588


gmtMinus5 : TimeZone
gmtMinus5 =
    customZone { moment = epochMoment, dateTime = { date = fromJDN 2440587, time = h24 19 |> m 0 } } []


gmtPlus12 : TimeZone
gmtPlus12 =
    customZone { moment = epochMoment, dateTime = { date = fromJDN 2440588, time = h24 12 |> m 0 } } []
