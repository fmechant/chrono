module Chrono.DateTests exposing (all)

import Chrono.Date exposing (..)
import Chrono.Moment as Moment
import Chrono.TimeZone as TimeZone
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
                        |> fromMoment TimeZone.utc
                        |> toJDN
                        |> Expect.equal 2451545
            , test "Epoch moment in GMT-5 is JDN 2,440,587" <|
                \() ->
                    epochMoment
                        |> fromMoment (TimeZone.customZone (-5 * 60) [])
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
                        |> toNoon TimeZone.utc
                        |> Expect.equal (Moment.fromMsSinceEpoch 43200000)
            , test "toNoon on January 1, 1970 is 0 in GMT+12." <|
                \() ->
                    firstJanuary1970
                        |> toNoon (TimeZone.customZone (12 * 60) [])
                        |> Expect.equal (Moment.fromMsSinceEpoch 0)
            ]
        ]


{-| The epoch moment is Thurday, January 1, 1970.
-}
epochMoment : Moment.Moment
epochMoment =
    Moment.fromMsSinceEpoch 0


firstJanuary1970 : Date
firstJanuary1970 =
    epochMoment
        |> fromMoment TimeZone.utc


fuzzDate : Fuzz.Fuzzer Date
fuzzDate =
    Fuzz.map fromJDN Fuzz.int


fuzzDuration : Fuzz.Fuzzer Duration
fuzzDuration =
    Fuzz.map days Fuzz.int


fuzzThursday : Fuzz.Fuzzer Date
fuzzThursday =
    Fuzz.map (\i -> fromJDN ((i // 7) * 7 + 3)) Fuzz.int
