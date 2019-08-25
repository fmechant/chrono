# Working with dates, times and moments in time

When you want to work with dates and time, you realize that it is surprisingly complex. You need to learn about time zones and posix time and calendars, but you only simply want to use a Date.
This library aspires to allow you to work with dates and times, preventing you from making mistakes without having to know all the intrinsics about the Date/Time problems.

# Things to know

Still, there are things you should be aware of to be able to work with dates and times effectively. Two concepts to know about.

## Moments in time

The first concept is moments in time. What [elm/time][coretime] calls `Posix` and what [Abseil][abseil] calls _Absolute Time_. It is the moment in time that something happened.
To improve understanding, let's look at the moment of time Neil Armstrong first set foot on the moon. If we look at [wikipedia][wikiapollo], it says that happened on
July 21, 1969 at 02:56 UTC. When viewing live in Europe, you could have seen that on July 21 at 04:56. In New York, that would have been on July 20 at 22:56. Remark that even the date is different.
To be able to represent a moment in time, we pick a moment in the past, and work relative from that.

[coretime]: https://package.elm-lang.org/packages/elm/time/latest
[abseil]: https://abseil.io/docs/cpp/guides/time
[wikiapollo]: https://en.wikipedia.org/wiki/Apollo_11

## Date and Time

The second concept is dates and times. What [elm/time][coretime] calls _Human Time_ and what [Abseil][abseil] calls _Civil Time_. A specific date is an abstract concept that is not directly linked to a moment in time.
To improve understanding, let's look at the celebration of New Year 2020. In the diagram, we see at what moments in time New Year is celebrated.

TODO: Add diagram of new years in moments of time

In this library, we depart from most other Time libraries, in that we consider a Date as a specific concept, irrelative of moments in time. We avoid using UTC to mix the concepts.
To be able to represent a date, we pick a date in the past, and work relative from that.

## Conversion

Although there is some correlation with moments in time and date and time, we can clearly see it is not a simple relationship. To convert from a moment in time to a date and time, we need a timezone. That is why usually UTC is used to 'fix' the conversion problem, but we believe it creates more problems than it solved.

# Examples

TODO: create some simple example for using the library for things that users need all the time

## A Date Selector

TODO

## A Recurrent Meeting

A good example of how to deal with preventing the mixture of concepts is when
creating a recurrent meeting:
Suppose we want to make a meeting recurrent for the next three weeks.
Since a meeting is at a specific moment in time, its representation is a moment.
Weeks is a concept of the date and time model. Here is how we do that using this
library:

1. get the date and time of the moment in the specific time zone
2. get the dates and times fast forwarded a week three times
3. convert the dates and times to moments in the specific time zone

```elm
import Chrono.Date as Date
import Chrono.Time as Time
import Chrono.TimeZone as TimeZone exposing (TimeZone)
import Chrono.Moment exposing (Moment)
import Chrono.DateAndTime as DateAndTime
import Chrono.GregorianCalendar as Cal

recurrent: Int -> Date.Duration -> TimeZone -> Moment -> List Moment
recurrent times duration timeZone moment =
    let
        { date, time } = DateAndTime.fromMoment timeZone moment
    in
    date
        |> Date.collect times duration
        |> List.map (DateAndTime.withTime time)
        |> List.map (DateAndTime.toMoment timeZone)

{-| Brussels time goes to summer time on 31 March 2019.-}
brusselsTimeZone : TimeZone
brusselsTimeZone =
    TimeZone.customZone 60 [{start = 1553994000000, offset = 120}]

inBrussels : { day : Int, month : Cal.Month, year : Int, hour : Int, minute : Int } -> Moment
inBrussels =
    Cal.fromDayMonthYearHourMinute >> (DateAndTime.toMoment brusselsTimeZone)

recurrent 3 (Date.weeks 1) brusselsTimeZone (inBrussels {day = 29, month = Cal.March, year = 2019, hour = 13, minute = 0})
--> [
-->    inBrussels {day = 5, month = Cal.April, year = 2019, hour = 13, minute = 0},
-->    inBrussels {day = 12, month = Cal.April, year = 2019, hour = 13, minute = 0},
-->    inBrussels {day = 19, month = Cal.April, year = 2019, hour = 13, minute = 0}
--> ]
```

So, instead of mixing the concept, we explicitly use the correct models, so we have
predictable results. Even when the weeks include a change of the time offset
(daylight savings time).
If we were to naively add 7 _ 24 _ 60 _ 60 _ 1000 ms to the moment, we introduced
an error when the time offset changes. Intuitively, if we fast forward a meeting
a week, we want the hour to stay the same.
If we were to represent the meeting in a date and time, we again introduced an error,
because if somebody from a different time zone where to participate in the meeting,
he would be late, or early.

We hope this library makes you think what it is exactly that you are trying to
represent, and then use the relevant model, so subtle errors are avoided.
