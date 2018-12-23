module Chrono.GregorianCalendar exposing (toDate, toMonthNumber)

import Chrono.Date as Date
import Time


{-| Convert a year, month and day on the gregorian calendar to a date.
-}
toDate : Int -> Time.Month -> Int -> Date.Date
toDate y month d =
    let
        m =
            toMonthNumber month
    in
    Date.fromJDN <| (1461 * (y + 4800 + (m - 14) // 12)) // 4 + (367 * (m - 2 - 12 * ((m - 14) // 12))) // 12 - (3 * ((y + 4900 + (m - 14) // 12) // 100)) // 4 + d - 32075


{-| Convert the month to a number representing the month.
1 is January, 12 is December.
-}
toMonthNumber : Time.Month -> Int
toMonthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
