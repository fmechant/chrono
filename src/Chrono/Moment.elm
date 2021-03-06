module Chrono.Moment exposing
    ( Moment, now
    , intoFuture, intoPast
    , earliest, chronologicalComparison
    , fromMsSinceEpoch, toMsAfterEpoch
    , TimeLapse(..), Direction(..), every, elapsed
    , hours, minutes, seconds, milliseconds, and
    , timeLapseView
    )

{-| Module for working with moments in time and time lapses. Hours, minutes,
seconds make sense here.

If you are looking for concepts like days or weeks, look into [Date][./Date].
Months or years are used in the [GregorianCalendar][./GregorianCalendar]
module.

If you are looking for [TimeZone][./Date#TimeZone], it is part of the Date
module, because time zones define the mapping between a moment and a date/time.


# Moments

@docs Moment, now


## Time Travel

@docs intoFuture, intoPast


## Comparing Moments

@docs earliest, chronologicalComparison


## Exchanging Moments with Other Systems

@docs fromMsSinceEpoch, toMsAfterEpoch


# Time Lapses

@docs TimeLapse, Direction, every, elapsed


## Defining Time Lapses

@docs hours, minutes, seconds, milliseconds, and


## Viewwing Time Lapses

@docs timeLapseView

-}

import Task exposing (Task)
import Time as CoreTime


{-| A specific moment in time. For example, the moment you started reading this
sentence.

What we call `Moment`, is what [elm/time][coretime] calls `Posix` and what
[Abseil][abseil] calls _Absolute Time_.

To improve understanding, let's look at the moment of time Neil Armstrong first
set foot on the moon. If we look at [wikipedia][wikiapollo], it says that
happened on July 21, 1969 at 02:56 UTC. When viewing live in Europe, you could
have seen that on July 21 at 04:56. In New York, that would have been on July 20
at 22:56. Remark that even the date is different.

To be able to represent a moment in time, we pick a moment in the past (the
epoch), and work relative from that.

[coretime]: https://package.elm-lang.org/packages/elm/time/latest
[abseil]: https://abseil.io/docs/cpp/guides/time
[wikiapollo]: https://en.wikipedia.org/wiki/Apollo_11

-}
type Moment
    = Moment Int


{-| Get the moment when this task is run.

This is typically used with the Elm architecture, like this:

    import Task

    type Msg
        = SystemGotNow Moment

    update msg model =
        case msg of
            _ ->
                ( model, Task.perform SystemGotNow now )

-}
now : Task x Moment
now =
    Task.map (fromMsSinceEpoch << CoreTime.posixToMillis) CoreTime.now


{-| Get the moment that occured the number of milliseconds after the epoch.

Typically only used when receiving a moment that was previously exported.
Avoid using this for calculations, let this library do the hard work for you.

-}
fromMsSinceEpoch : Int -> Moment
fromMsSinceEpoch ms =
    Moment ms


{-| Get the number of milliseconds after the epoch that this moment occured.

Typically only used for exporting the moment.
Avoid using this for calculations, let this library do the hard work for you.

-}
toMsAfterEpoch : Moment -> Int
toMsAfterEpoch (Moment ms) =
    ms


{-| Move the moment into the future for a time lapse.

If you want to move days, weeks, months or years, use Date or GregorianCalendar
for that.

-}
intoFuture : TimeLapse -> Moment -> Moment
intoFuture (TimeLapse lapseInMs) (Moment momentInMs) =
    Moment <| momentInMs + lapseInMs


{-| Move the moment into the past for a time lapse.

If you want to move days, weeks, months or years, use Date or GregorianCalendar
for that.

-}
intoPast : TimeLapse -> Moment -> Moment
intoPast (TimeLapse lapseInMs) (Moment momentInMs) =
    Moment <| momentInMs - lapseInMs


{-| Get the moment that happened first.
-}
earliest : Moment -> Moment -> Moment
earliest (Moment ms) (Moment ms2) =
    Moment (min ms ms2)


{-| Compare two moments chronologically. Typically used with `List.sortWith`.

    import List

    let
        base = fromMsSinceEpoch 0
        later = intoFuture (minutes 5) base
        earlier = intoPast (hours 20) base
    in
    [earlier, base, later] == List.sortWith chronologicalComparison [later, earlier, base]
    --> True

-}
chronologicalComparison : Moment -> Moment -> Order
chronologicalComparison (Moment m) (Moment n) =
    Basics.compare m n



---- TimeLapse ----


{-| TimeLapses represent a specific time lapse between two moments. For example,
the time lapse between starting to read the first sentence, and the start of
reading this sentence.

It is represented in the moment module, because we are thinking about actual
elaps of specific seconds, minutes and hours.

**TimeLapse has no way of describing days,** because one day is not always 24
hours. For example, going 24 hours into the future is not the same as going a
day into the future. In Europe it is only the same in about 363 days a year,
because of daylight time savings.

If you want to describe duration of days, weeks, months or years, use Date or
GregorianCalendar for that.

-}
type TimeLapse
    = TimeLapse Int


{-| Direction represents the relative position of one moment regarding another
moment, whether it is into the future, or into the past.
-}
type Direction
    = IntoTheFuture
    | IntoThePast


{-| A time lapse of some milliseconds.
Only use positive values, if you want your code to be predictable.
-}
milliseconds : Int -> TimeLapse
milliseconds value =
    TimeLapse value


{-| A time lapse of some seconds.
Only use positive values, if you want your code to be predictable.
-}
seconds : Int -> TimeLapse
seconds value =
    milliseconds <| value * 1000


{-| A time lapse of some minutes.
Only use positive values, if you want your code to be predictable.
-}
minutes : Int -> TimeLapse
minutes value =
    seconds <| value * 60


{-| A time lapse of some hours.
Only use positive values, if you want your code to be predictable.
-}
hours : Int -> TimeLapse
hours value =
    minutes <| value * 60


{-| Combine two time lapses.

It has an odd signiture to be able to efficiently use it using the pipe (|>)
operator.

Example:

    hours 2
        |> and minutes 45
        |> timeLapseView
    --> { hours = 2, minutes = 45, seconds = 0, milliseconds = 0}

-}
and : (Int -> TimeLapse) -> Int -> TimeLapse -> TimeLapse
and fct value (TimeLapse timeLapse) =
    let
        (TimeLapse toAdd) =
            fct value
    in
    TimeLapse (timeLapse + toAdd)


{-| Show the time lapse split up in milliseconds, seconds, minutes and hours.

Typically used to create your own specific view of the time lapse.

    let
        myTimeLapseView timeLapse =
            let
                {hours, minutes} = timeLapseView timeLapse
            in
            String.fromInt hours ++ ":" ++ String.fromInt minutes
    in
    hours 5
        |> and minutes 45
        |> myTimeLapseView
        --> "5:45"

-}
timeLapseView : TimeLapse -> { milliseconds : Int, seconds : Int, minutes : Int, hours : Int }
timeLapseView (TimeLapse timeLapse) =
    let
        -- Subtract the whole part, when dividing by the factor, and return the whole part, and the remaining value.
        substractWhole : Int -> Int -> ( Int, Int )
        substractWhole value factor =
            let
                whole =
                    value // factor
            in
            ( whole, value - whole * factor )

        ( wholeHours, withoutHours ) =
            substractWhole timeLapse 3600000

        ( wholeMinutes, withoutMinutes ) =
            substractWhole withoutHours 60000

        ( wholeSeconds, withoutSeconds ) =
            substractWhole withoutMinutes 1000
    in
    { milliseconds = withoutSeconds, seconds = wholeSeconds, minutes = wholeMinutes, hours = wholeHours }


{-| How much time has elapsed between the moments.

The result is a time lapse, with the indication whether one moment is in the future
or in the past regarding to the other moment.

-}
elapsed : Moment -> Moment -> ( TimeLapse, Direction )
elapsed (Moment from) (Moment to) =
    let
        diff =
            to - from

        dir =
            if diff < 0 then
                IntoThePast

            else
                IntoTheFuture
    in
    ( TimeLapse <| abs diff, dir )


{-| Get the current moment, every time lapse.

If it is unclear to you why it returns a Sub, please review the Elm architecture.

-}
every : TimeLapse -> (Moment -> msg) -> Sub msg
every (TimeLapse timeLapse) function =
    CoreTime.every (toFloat timeLapse) (CoreTime.posixToMillis >> fromMsSinceEpoch >> function)
