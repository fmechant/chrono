# Date, Time, Moment

`chrono` simplifies working with dates, time and moments in time.

The particular goals of this library are:
- Make working with dates, time and moments as simple as possible, but not simpler.
- Prevent common mistakes by making you think what you actually mean, a date or a moment.
- Be able to use without knowing all the complexities of dates and time.
- Make you have fun with time travel :)

## Concepts and terminology

Some important concepts and terminology to understand.

### Moment

Suppose you live in New York and you want to schedule a video call with a friend
in Amsterdam. When you agree on a date and time, you have to realize that you
are both in a different time zone. Meeting on 2020-05-20 at 16:00 does not make
sense without the time zone. If you say 16:00 in New York, your friend will know
it to be 22:00 in Amsterdam.

You are actually deciding on a specific `Moment` to meet. The use of date and time
is convenient for us, because *1590004800000* is difficult to work with.

### Time

Suppose you are sportive and you schedule a 30 minute run every morning at 8:00.
When you go to visit your friend in Amsterdam, you still want to have your run
at 8:00, and not at 14:00. It is a specific `Time` you mean.

### Date

You both celebrated New Year's at the start of 1 January 2020. It was a different
moment for both of you, but it was the same `Date`.

### Time Zone

For your watch to be able to notify you on 2020-05-21 that it is time to start
running, it needs to convert 2020-05-21 and 8:00 to a specific moment. It needs
a `TimeZone` to do that. If it didn't have that, it would notify you at 14:00 in
Amsterdam, not what you want.

## Examples

### Get current Date and Time

You can see what date it is by using `now`.

By the way, it uses the current time zone under the hood.

```elm
import Chrono.Date as Date exposing (DateAndTime)
import Task

type Msg
    = SystemGotDate DateAndTime
    
getDate : Cmd Msg
getDate =
    Task.perform SystemGotDate Date.now
```

### Video call in New York And Amsterdam

Converting date and time to a different time zone.

```elm
import Chrono.Date as Date exposing (h24, m)
import Chrono.GregorianCalendar as Cal exposing (Month(..))
import Chrono.Moment as Moment exposing (Moment)

let
    newYorkZone = Date.customZone { moment = Moment.fromMsSinceEpoch 0, dateTime = { date = Date.fromJDN 2440587, time = h24 20 |> m 0 } } []
    amsterdamZone = Date.customZone { moment = Moment.fromMsSinceEpoch 0, dateTime = { date = Date.fromJDN 2440588, time = h24 2 |> m 0 } } []
in
{ date = Cal.toDate { year = 2020, month = May, day = 20}, time = h24 16 |> m 0 }
    |> Date.toMoment newYorkZone
    |> Date.toDateAndTime amsterdamZone
    --> { date = Cal.toDate { year = 2020, month = May, day = 20}, time = h24 22 |> m 0 }
```

### Simple Time Travel

Moving backwards in time.

```elm
import Chrono.Date as Date exposing (Date)
import Chrono.GregorianCalendar as Cal exposing (intoPast, Month(..), weeks)

let
    twoWeeksEarlier: Date -> Date
    twoWeeksEarlier date =
        date 
            |> Cal.travel (intoPast (weeks 2))
in
Cal.toDate { day = 15, month = January, year = 2020  }
    |> twoWeeksEarlier
--> Cal.toDate { day = 1, month = January, year = 2020  }
```

### Moving months

When working with months, we need a `MoveStrategy` so the time traveller
knows what to do with impossible dates.

The `stayInSameMonth` strategy finds a correct date as close as possible, within
the same month, so 31 February would become 28 or 29 February, and not 3 or 4
March.

```elm
import Chrono.Date as Date exposing (Date)
import Chrono.GregorianCalendar as Cal exposing (intoFuture, Month(..), months)

let
    dateOfBirth: Date -> Date
    dateOfBirth date =
        date 
            |> Cal.travel (intoFuture (months 9 Cal.stayInSameMonth))
in
Cal.toDate { day = 30, month = May, year = 2019  }
    |> dateOfBirth
--> Cal.toDate { day = 29, month = February, year = 2020  }
```

### A Recurrent Meeting

A good example of how to deal with preventing the misuse of concepts is when
creating a recurrent meeting:
Suppose we want to make a meeting recurrent for the next three weeks.
Since a meeting is at a specific moment in time, its representation is a moment.

Action plan:
1. Get the date and time of the moment in the specific time zone.
2. Get the dates and time fast forwarded a week three times.
3. Convert the dates and time to moments in the specific time zone.

PS: Brussels time goes to summer time on 31 March 2019.

```elm
import Chrono.Date as Date exposing (h24, intoFuture, m)
import Chrono.Moment as Moment exposing (Moment)
import Chrono.GregorianCalendar as Cal exposing (Month(..))

inBrussels : { day : Int, month : Month, year : Int, hour : Int, minute : Int } -> Moment
inBrussels { day, month, year, hour, minute} =
    { day = day, month = month, year = year}
        |> Cal.toDate
        |> Date.withTime (h24 hour |> m minute)
        |> Date.toMoment brusselsTimeZone

brusselsTimeZone : Date.TimeZone 
brusselsTimeZone =
    Date.customZone { moment = Moment.fromMsSinceEpoch 0, dateTime = { date = Date.fromJDN 2440588, time = h24 1 |> m 0 } }
        [ { start = { moment = Moment.fromMsSinceEpoch 1553994000000, dateTime = { date = Date.fromJDN 2458574, time = h24 3 |> m 0 } } } ]

let
    recurrent: { times: Int, duration: Date.Duration, timeZone: Date.TimeZone} -> Moment -> List Moment
    recurrent { times, duration, timeZone} moment =
        let
            { date, time } = Date.toDateAndTime timeZone moment
        in
        date
            |> Date.collect times (intoFuture duration)
            |> List.map (Date.withTime time)
            |> List.map (Date.toMoment timeZone)
in
recurrent { times = 3, duration = (Date.weeks 1), timeZone = brusselsTimeZone}
    (inBrussels {day = 22, month = Cal.March, year = 2019, hour = 13, minute = 0})
--> [
-->    inBrussels {day = 29, month = March, year = 2019, hour = 13, minute = 0},
-->    inBrussels {day = 5, month = April, year = 2019, hour = 13, minute = 0},
-->    inBrussels {day = 12, month = April, year = 2019, hour = 13, minute = 0}
--> ]
```

So if we use the correct models, we get predictable results. Even when the weeks
include a change of the time offset (daylight savings time).
If we were to naively add 7 \* 24 \* 60 \* 60 \* 1000 ms to the moment, we introduced
an error when the time offset changes.

**Intuitively**, if we fast forward a meeting
a week, we want the hour to stay the same.

If we were to represent the meeting in a date and time, we again introduced an error,
because if somebody from a different time zone where to participate in the meeting,
he would be late, or early.


## Remarks

In this library, we depart from most other date/time libraries. How?
- We consider a Date as a specific concept, irrelative of moments in time. We avoid using UTC to mix the concepts.
To be able to represent a date, we pick a date in the past (epoch date), and work relative from that.
- TimeZone is completely independent of UTC. It is defined as periods,
with each period defining a (bijective) mapping between moment and date/time. This
is something very new to date/time libraries, but when I started
thinking this way, everything got way simpler.

We hope this library makes you think what it is exactly that you are trying to
represent, and then use the relevant concept, so subtle errors are avoided.
