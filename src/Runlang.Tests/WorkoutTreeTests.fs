module WorkoutTreeTests

open FsUnit.Xunit
open Utils
open Xunit
open Interval

[<Fact>]
let ``To intervals flats all steps into an interval list`` () =
    let expected =
        [ "#1 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          "#2 Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
          "#3 Time: 00:01:12, Distance: 100m, Pace: 12:00/km";
          "#4 Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
          "#5 Time: 00:01:12, Distance: 100m, Pace: 12:00/km";
          "#6 Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
          "#7 Time: 00:01:12, Distance: 100m, Pace: 12:00/km";
          "#8 Time: 00:07:00, Distance: 1.00km, Pace: 7:00/km" ]

    let intervals =
        parseToIntervals "1km TR + 3x(400m FO + 100m CL) + 1km LE"
        |> Interval.listToString

    intervals |> should equal expected
