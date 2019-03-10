module Chrono.Moment exposing
    ( Duration
    , Moment
    , and
    , chronologicalComparison
    , elapsed
    , fromMsSinceEpoch
    , hours
    , intoFuture
    , intoFutureForZone
    , intoPast
    , intoPastForZone
    , milliseconds
    , minutes
    , now
    , seconds
    , toMsAfterEpoch
    , viewDuration
    )

{-| The moment model represents specific moments in time. For example the moment you
first started reading this sentence.
-}

import Chrono.TimeZone as TimeZone exposing (TimeZone)
import Task exposing (Task)
import Time as CoreTime


{-| A specific moment in time.
-}
type Moment
    = Moment Int


{-| Get the current moment when this task is run.
-}
now : Task x Moment
now =
    Task.map (fromMsSinceEpoch << CoreTime.posixToMillis) CoreTime.now


{-| Get the moment that occured the number of milliseconds after the epoch.

Typically only used when receiving a moment that was previously exported.

-}
fromMsSinceEpoch : Int -> Moment
fromMsSinceEpoch ms =
    Moment ms


{-| Get the number of milliseconds after the epoch that this moment occured.

Do not use this for calculations, only for exporting the moment.

-}
toMsAfterEpoch : Moment -> Int
toMsAfterEpoch (Moment ms) =
    ms


{-| Move the moment into the future for a duration.

Do not use this to move days, weeks, or months. Use the date and time model for that.

-}
intoFuture : Duration -> Moment -> Moment
intoFuture (Duration durationInMs) (Moment momentInMs) =
    Moment <| momentInMs + durationInMs


{-| Move the moment into the past for a duration.

Do not use this to move days, weeks, or months. Use the date and time model for that.

-}
intoPast : Duration -> Moment -> Moment
intoPast (Duration durationInMs) (Moment momentInMs) =
    Moment <| momentInMs - durationInMs


{-| Compare two moments chronologically. Typically used with `List.sortWith`.

    import List

    base = fromMsSinceEpoch 0
    later = intoFuture (minutes 5) base
    earlier = intoPast (hours 20) base

    chronologicalComparison base later
    --> LT

    List.sortWith chronologicalComparison [later, earlier, base]
    --> [earlier, base, later]

-}
chronologicalComparison : Moment -> Moment -> Order
chronologicalComparison (Moment m) (Moment n) =
    Basics.compare m n


intoFutureForZone : TimeZone -> Moment -> Moment
intoFutureForZone zone moment =
    let
        ms =
            toMsAfterEpoch moment
    in
    fromMsSinceEpoch (ms + TimeZone.relevantOffset zone ms)


intoPastForZone : TimeZone -> Moment -> Moment
intoPastForZone zone moment =
    let
        ms =
            toMsAfterEpoch moment
    in
    fromMsSinceEpoch (ms - TimeZone.relevantOffset zone ms)



---- DURATION ----


{-| Duration represents a laps of time. It is represented in the moment model,
because we are thinking about actual elaps of specific milliseconds, seconds, minutes and hours.

It has no way of describing days, because that belongs in the Date and Time model.
For example, moving 24 hours is not the same as moving a day. In Europe it is only
the same in about 363 days a year, because of daylight time savings.

-}
type Duration
    = Duration Int


{-| A duration of some milliseconds.
Only use positive values, if you want your code to be predictable.
-}
milliseconds : Int -> Duration
milliseconds value =
    Duration value


{-| A duration of some seconds.
Only use positive values, if you want your code to be predictable.
-}
seconds : Int -> Duration
seconds value =
    milliseconds <| value * 1000


{-| A duration of some minutes.
Only use positive values, if you want your code to be predictable.
-}
minutes : Int -> Duration
minutes value =
    seconds <| value * 60


{-| A duration of some hours.
Only use positive values, if you want your code to be predictable.
-}
hours : Int -> Duration
hours value =
    minutes <| value * 60


{-| Combine two durations.

It has an odd signiture to be able to efficiently use it using the pipe (|>) operator.
Example:

    hours 2
        |> and minutes 45
        |> viewDuration
    --> { hours: 2, minutes: 45, seconds: 0, milliseconds: 0}

-}
and : (Int -> Duration) -> Int -> Duration -> Duration
and fct value (Duration duration) =
    let
        (Duration toAdd) =
            fct value
    in
    Duration (duration + toAdd)


{-| Show the duration split up in milliseconds, seconds, minutes and hours.
-}
viewDuration : Duration -> { milliseconds : Int, seconds : Int, minutes : Int, hours : Int }
viewDuration (Duration duration) =
    let
        ( wholeHours, withoutHours ) =
            substractWhole duration 3600000

        ( wholeMinutes, withoutMinutes ) =
            substractWhole withoutHours 60000

        ( wholeSeconds, withoutSeconds ) =
            substractWhole withoutMinutes 1000
    in
    { milliseconds = withoutSeconds, seconds = wholeSeconds, minutes = wholeMinutes, hours = wholeHours }


{-| How much time has elapsed between the moments.

The result is a duration, without the indication whether one moment is in the future
or in the past regarding to the other moment.

-}
elapsed : Moment -> Moment -> Duration
elapsed (Moment from) (Moment to) =
    Duration <| abs <| to - from


{-| Get the current moment, every duration.

If it is unclear to you why it returns a Sub, please review the Elm architecture.

-}
every : Duration -> (Moment -> msg) -> Sub msg
every duration function =
    Sub.none



---- HELPER FUNCTIONS ----


{-| Subtract the whole part, when dividing by the factor, and return the whole part, and the remaining value.
-}
substractWhole : Int -> Int -> ( Int, Int )
substractWhole value factor =
    let
        whole =
            value // factor
    in
    ( whole, value - whole * factor )
