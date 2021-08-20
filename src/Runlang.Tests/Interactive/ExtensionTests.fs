module InteractiveExtensionsTests

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


[<Fact>]
let ``To intervals display progressive steps`` () =
    let expected =
        [ "#1 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km";
          "#2 Time: 00:04:50, Distance: 1.00km, Pace: 4:50/km";
          "#3 Time: 00:04:40, Distance: 1.00km, Pace: 4:40/km";
          "#4 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km";
          "#5 Time: 00:04:20, Distance: 1.00km, Pace: 4:20/km";
          "#6 Time: 00:04:10, Distance: 1.00km, Pace: 4:10/km";
          "#7 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km" ]

    let intervals =
        parseToIntervals "7km 5:00/km->4:00/km:1km"
        |> Interval.listToString

    intervals |> should equal expected

[<Fact>]
let ``To intervals work with nested progressive steps`` () =
    let expected =
        [ "#1 Time: 00:05:00, Distance: 1.07km, Pace: 4:40/km";
          "#2 Time: 00:00:48, Distance: 200m, Pace: 4:00/km";
          "#3 Time: 00:01:03, Distance: 200m, Pace: 5:15/km";
          "#4 Time: 00:01:18, Distance: 200m, Pace: 6:30/km";
          "#5 Time: 00:01:33, Distance: 200m, Pace: 7:45/km";
          "#6 Time: 00:01:48, Distance: 200m, Pace: 9:00/km";
          "#7 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          "#8 Time: 00:00:48, Distance: 200m, Pace: 4:00/km";
          "#9 Time: 00:01:03, Distance: 200m, Pace: 5:15/km";
          "#10 Time: 00:01:18, Distance: 200m, Pace: 6:30/km";
          "#11 Time: 00:01:33, Distance: 200m, Pace: 7:45/km";
          "#12 Time: 00:01:48, Distance: 200m, Pace: 9:00/km";
          "#13 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          "#14 Time: 00:00:48, Distance: 200m, Pace: 4:00/km";
          "#15 Time: 00:01:03, Distance: 200m, Pace: 5:15/km";
          "#16 Time: 00:01:18, Distance: 200m, Pace: 6:30/km";
          "#17 Time: 00:01:33, Distance: 200m, Pace: 7:45/km";
          "#18 Time: 00:01:48, Distance: 200m, Pace: 9:00/km";
          "#19 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km" ]

    let intervals =
        parseToIntervals "5min 4:40/km + 3x(1km FTS->TR:200m + 1km TR)"
        |> Interval.listToString

    intervals |> should equal expected

[<Fact>]
let ``To intervals work with deeply nested workout`` () =
    let expected =
        ["#1 Time: 00:00:54, Distance: 100m, Pace: 9:00/km";
 "#2 Time: 00:01:00, Distance: 250m, Pace: 4:00/km";
 "#3 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
 "#4 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#5 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#6 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#7 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#8 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#9 Time: 00:01:00, Distance: 111m, Pace: 9:00/km";
 "#10 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
 "#11 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#12 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#13 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#14 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#15 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#16 Time: 00:01:00, Distance: 111m, Pace: 9:00/km";
 "#17 Time: 00:01:00, Distance: 83m, Pace: 12:00/km";
 "#18 Time: 00:00:54, Distance: 100m, Pace: 9:00/km";
 "#19 Time: 00:01:00, Distance: 250m, Pace: 4:00/km";
 "#20 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
 "#21 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#22 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#23 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#24 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#25 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#26 Time: 00:01:00, Distance: 111m, Pace: 9:00/km";
 "#27 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
 "#28 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#29 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#30 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#31 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#32 Time: 00:00:04, Distance: 20m, Pace: 3:00/km";
 "#33 Time: 00:01:00, Distance: 111m, Pace: 9:00/km";
 "#34 Time: 00:01:00, Distance: 83m, Pace: 12:00/km";
 "#35 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
 "#36 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
 "#37 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
 "#38 Time: 00:00:10, Distance: 67m, Pace: 2:30/km"]

    let intervals =
        parseToIntervals """2x(100m TR + 1min FTS + 2x(1km TR + 5x(20m MAX) +
        1min TR) + 1min CL) + 3x(1km FTS) + 10s 2:30/km"""
        |> Interval.listToString

    intervals |> should equal expected
