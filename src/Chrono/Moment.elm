module Chrono.Moment exposing
    ( Direction(..)
    , Moment
    , TimeLapse(..)
    , and
    , chronologicalComparison
    , earliest
    , elapsed
    , fromMsSinceEpoch
    , hours
    , intoFuture
    , intoPast
    , milliseconds
    , minutes
    , now
    , seconds
    , timeLapseView
    , toMsAfterEpoch
    )

{-| The moment model represents specific moments in time. For example, the moment you
first started reading this sentence.
-}

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


{-| Move the moment into the future for a time lapse.

Do not use this to move days, weeks, or months. Use Date and GregorianCalendar for that.

-}
intoFuture : TimeLapse -> Moment -> Moment
intoFuture (TimeLapse lapseInMs) (Moment momentInMs) =
    Moment <| momentInMs + lapseInMs


{-| Move the moment into the past for a time lapse.

Do not use this to move days, weeks, or months. Use Date and GregorianCalendar for that.

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


{-| TimeLapse represents a lapse of time. It is represented in the moment module,
because we are thinking about actual elaps of specific milliseconds, seconds, minutes and hours.

TimeLapse has no way of describing days, because one day is not always 24 hours.
For example, moving 24 hours is not the same as moving a day. In Europe it is only
the same in about 363 days a year, because of daylight time savings.
Use Date and GregorianCalendar for describing an elaps of days, weeks, months, years.

-}
type TimeLapse
    = TimeLapse Int


{-| Direction represents the relative position of one moment regarding another moment,
whether it is into the future, or into the past.
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

It has an odd signiture to be able to efficiently use it using the pipe (|>) operator.
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

-}
timeLapseView : TimeLapse -> { milliseconds : Int, seconds : Int, minutes : Int, hours : Int }
timeLapseView (TimeLapse timeLapse) =
    let
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


{-| Get the current moment, every duration.

If it is unclear to you why it returns a Sub, please review the Elm architecture.

-}
every : TimeLapse -> (Moment -> msg) -> Sub msg
every (TimeLapse timeLapse) function =
    CoreTime.every (toFloat timeLapse) (CoreTime.posixToMillis >> fromMsSinceEpoch >> function)



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
