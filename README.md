# Crono

Work with moments in time, dates and times of day in Elm.

## Motivation

Working accurately with dates and times is surprisingly complex.
This package does not hide the complexity, but makes it easier to work with.

Instead of mixing the concepts of moments in time and date and times, we choose
to make the distinction explicit.

The moment model represents specific moments in time. For example the moment you
first started reading this sentence.

The date and time model represents the abstract concepts of date and time. For
example New Years Day in 2019.

To convert from one model to the other, we need a time zone. Everybody knows New
Years is celebrated earlier in Europe than it is in America, for example.

A good example of how to deal with preventing the mixture of concepts is when
creating a recurrent meeting:
Suppose we want to make a meeting recurrent for the next three weeks.
Since a meeting is at a specific moment in time, its representation is a moment.
Weeks is a concept of the date and time model. Here is how we do that using this
library:
1. get the date and time of the moment in the specific time zone
2. get the dates and times fast forwarded a week three times
3. convert the dates and times to moments in the specific time zone

So, instead of mixing the concept, we explicitly use the correct models, so we have
predictable results. Even when the weeks include a change of the time offset
(daylight savings time).
If we were to naively add 7 * 24 * 60 * 60 * 1000 ms to the moment, we introduced
an error when the time offset changes. Intuitively, if we fast forward a meeting
a week, we want the hour to stay the same.
If we were to represent the meeting in a date and time, we again introduced an error,
because if somebody from a different time zone where to participate in the meeting,
he would be late, or early.

We hope this library makes you think what it is exactly that you are trying to
represent, and then use the relevant model, so subtle errors are avoided.


## Examples

TODO

Here is an example of notifying the user 10 minutes before a scheduled meeting
is about to happen:

```elm
shouldNotify : Time.Zone -> Moment -> Date             
```


