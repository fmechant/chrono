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
        [ describe "fromMoment should use correct conversion to JDN."
            [ test "1 January 1970 is JDN 2,440,588." <|
                \() ->
                    firstJanuary1970
                        |> toJDN
                        |> Expect.equal 2440588
            , test "January 1, 2000 is JDN 2,451,545." <|
                \() ->
                    Moment.fromMsSinceEpoch 946684800000
                        |> fromMoment Moment.utc
                        |> toJDN
                        |> Expect.equal 2451545
            , test "Epoch moment in GMT-5 is JDN 2,440,587" <|
                \() ->
                    epochMoment
                        |> fromMoment (Moment.customZone (-5 * 60) [])
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
                        |> toNoon Moment.utc
                        |> Expect.equal (Moment.fromMsSinceEpoch 43200000)
            , test "toNoon on January 1, 1970 is 0 in GMT+12." <|
                \() ->
                    firstJanuary1970
                        |> toNoon (Moment.customZone (12 * 60) [])
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
                        |> viewDuration
                        |> Expect.equal { days = 5, weeks = 0 }
            , test "should show 1 week for 7 days" <|
                \() ->
                    days 7
                        |> viewDuration
                        |> Expect.equal { days = 0, weeks = 1 }
            , fuzz2 (Fuzz.intRange 0 6) (Fuzz.intRange 0 10000) "should work out days and weeks" <|
                \someDays someWeeks ->
                    days someDays
                        |> and weeks someWeeks
                        |> viewDuration
                        |> Expect.equal { days = someDays, weeks = someWeeks }
            ]
        , describe "elapsed"
            [ fuzz2 fuzzDate fuzzDateDuration "should return a duration that gives the date if we move it into the future" <|
                \date duration ->
                    date
                        |> intoFuture duration
                        |> elapsed date
                        |> Expect.equal duration
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
                    |> List.sortWith chronologicalComparison
                    |> Expect.equal [ earlier, date, later ]
        ]


{-| The epoch moment is Thurday, January 1, 1970.
-}
epochMoment : Moment.Moment
epochMoment =
    Moment.fromMsSinceEpoch 0


firstJanuary1970 : Date
firstJanuary1970 =
    epochMoment
        |> fromMoment Moment.utc
