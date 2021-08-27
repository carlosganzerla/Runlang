module IntervalEncoderTests

open FsUnit.Xunit
open Utils
open Xunit
open IntervalTree

[<Fact>]
let ``To string list trivial workout`` () =
    let expected = [ "Time: 00:45:00, Distance: 5.00km, Pace: 9:00/km" ]

    let intervals = parseToIntervalTree "5km TR" |> IntervalTree.toStringList

    intervals |> should equal expected

[<Fact>]
let ``To string list varying workout`` () =
    let expected =
        [ "Time: 00:32:00, Distance: 6.04km, Pace: 5:18/km";
          " -  Time: 00:10:00, Distance: 2.00km, Pace: 5:00/km";
          " -  Time: 00:03:00, Distance: 600m, Pace: 5:00/km";
          " -  Time: 00:02:00, Distance: 436m, Pace: 4:35/km";
          " -  Time: 00:14:00, Distance: 2.00km, Pace: 7:00/km";
          " -  Time: 00:03:00, Distance: 1.00km, Pace: 3:00/km" ]

    let intervals =
        parseToIntervalTree
            "10min 2km + 3min FO + 2min 4:35/km + 2km LE + 1km 3:00/km"
        |> IntervalTree.toStringList

    intervals |> should equal expected


[<Fact>]
let ``To string list simple repeat workout`` () =
    let expected =
        [ "Time: 00:44:00, Distance: 6.50km, Pace: 6:46/km";
          " -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  Time: 00:28:00, Distance: 4.50km, Pace: 6:13/km";
          " -  -  Time: 00:02:24, Distance: 400m, Pace: 6:00/km";
          " -  -  Time: 00:02:48, Distance: 400m, Pace: 7:00/km";
          " -  -  Time: 00:00:24, Distance: 100m, Pace: 4:00/km";
          " -  -  Time: 00:02:24, Distance: 400m, Pace: 6:00/km";
          " -  -  Time: 00:02:48, Distance: 400m, Pace: 7:00/km";
          " -  -  Time: 00:00:24, Distance: 100m, Pace: 4:00/km";
          " -  -  Time: 00:02:24, Distance: 400m, Pace: 6:00/km";
          " -  -  Time: 00:02:48, Distance: 400m, Pace: 7:00/km";
          " -  -  Time: 00:00:24, Distance: 100m, Pace: 4:00/km";
          " -  -  Time: 00:02:24, Distance: 400m, Pace: 6:00/km";
          " -  -  Time: 00:02:48, Distance: 400m, Pace: 7:00/km";
          " -  -  Time: 00:00:24, Distance: 100m, Pace: 4:00/km";
          " -  -  Time: 00:02:24, Distance: 400m, Pace: 6:00/km";
          " -  -  Time: 00:02:48, Distance: 400m, Pace: 7:00/km";
          " -  -  Time: 00:00:24, Distance: 100m, Pace: 4:00/km";
          " -  Time: 00:07:00, Distance: 1.00km, Pace: 7:00/km" ]

    let intervals =
        parseToIntervalTree "1km TR + 5x(400m MO + 400m LE + 100m FTS) + 1km LE"
        |> IntervalTree.toStringList

    intervals |> should equal expected

[<Fact>]
let ``To string list deeply nested workout`` () =
    let expected =
        [ "Time: 01:48:00, Distance: 12.00km, Pace: 9:00/km";
          " -  Time: 00:54:00, Distance: 6.00km, Pace: 9:00/km";
          " -  -  Time: 00:18:00, Distance: 2.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  -  Time: 00:18:00, Distance: 2.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  -  Time: 00:18:00, Distance: 2.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  Time: 00:54:00, Distance: 6.00km, Pace: 9:00/km";
          " -  -  Time: 00:18:00, Distance: 2.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  -  Time: 00:18:00, Distance: 2.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  -  Time: 00:18:00, Distance: 2.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          " -  -  -  Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km" ]

    let intervals =
        parseToIntervalTree "2x(3x(1x(2x(1km TR))))"
        |> IntervalTree.toStringList

    intervals |> should equal expected

[<Fact>]
let ``To string list progressive workout`` () =
    let expected =
        [ "Time: 00:10:30, Distance: 3.00km, Pace: 3:30/km";
          " -  Time: 00:03:40, Distance: 1.00km, Pace: 3:40/km";
          " -  Time: 00:03:30, Distance: 1.00km, Pace: 3:30/km";
          " -  Time: 00:03:20, Distance: 1.00km, Pace: 3:20/km" ]

    let intervals =
        parseToIntervalTree "3km 3:40/km->3:20/km:1km"
        |> IntervalTree.toStringList

    intervals |> should equal expected

[<Fact>]
let ``To string list complex workout`` () =
    let expected =
        [ "Time: 01:22:08, Distance: 17.80km, Pace: 4:37/km";
          " -  Time: 01:20:08, Distance: 17.50km, Pace: 4:35/km";
          " -  -  Time: 00:06:30, Distance: 1.00km, Pace: 6:30/km";
          " -  -  -  Time: 00:01:48, Distance: 200m, Pace: 9:00/km";
          " -  -  -  Time: 00:01:33, Distance: 200m, Pace: 7:45/km";
          " -  -  -  Time: 00:01:18, Distance: 200m, Pace: 6:30/km";
          " -  -  -  Time: 00:01:03, Distance: 200m, Pace: 5:15/km";
          " -  -  -  Time: 00:00:48, Distance: 200m, Pace: 4:00/km";
          " -  -  Time: 00:20:24, Distance: 6.00km, Pace: 3:24/km";
          " -  -  -  Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
          " -  -  -  Time: 00:04:48, Distance: 1.60km, Pace: 3:00/km";
          " -  -  -  Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
          " -  -  -  Time: 00:04:48, Distance: 1.60km, Pace: 3:00/km";
          " -  -  -  Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
          " -  -  -  Time: 00:04:48, Distance: 1.60km, Pace: 3:00/km";
          " -  -  Time: 00:06:10, Distance: 750m, Pace: 8:13/km";
          " -  -  -  Time: 00:00:14, Distance: 100m, Pace: 2:20/km";
          " -  -  -  Time: 00:01:00, Distance: 50m, Pace: 20:00/km";
          " -  -  -  Time: 00:00:14, Distance: 100m, Pace: 2:20/km";
          " -  -  -  Time: 00:01:00, Distance: 50m, Pace: 20:00/km";
          " -  -  -  Time: 00:00:14, Distance: 100m, Pace: 2:20/km";
          " -  -  -  Time: 00:01:00, Distance: 50m, Pace: 20:00/km";
          " -  -  -  Time: 00:00:14, Distance: 100m, Pace: 2:20/km";
          " -  -  -  Time: 00:01:00, Distance: 50m, Pace: 20:00/km";
          " -  -  -  Time: 00:00:14, Distance: 100m, Pace: 2:20/km";
          " -  -  -  Time: 00:01:00, Distance: 50m, Pace: 20:00/km";
          " -  -  Time: 00:07:00, Distance: 1.00km, Pace: 7:00/km";
          " -  -  Time: 00:06:30, Distance: 1.00km, Pace: 6:30/km";
          " -  -  -  Time: 00:01:48, Distance: 200m, Pace: 9:00/km";
          " -  -  -  Time: 00:01:33, Distance: 200m, Pace: 7:45/km";
          " -  -  -  Time: 00:01:18, Distance: 200m, Pace: 6:30/km";
          " -  -  -  Time: 00:01:03, Distance: 200m, Pace: 5:15/km";
          " -  -  -  Time: 00:00:48, Distance: 200m, Pace: 4:00/km";
          " -  -  Time: 00:20:24, Distance: 6.00km, Pace: 3:24/km";
          " -  -  -  Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
          " -  -  -  Time: 00:04:48, Distance: 1.60km, Pace: 3:00/km";
          " -  -  -  Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
          " -  -  -  Time: 00:04:48, Distance: 1.60km, Pace: 3:00/km";
          " -  -  -  Time: 00:02:00, Distance: 400m, Pace: 5:00/km";
          " -  -  -  Time: 00:04:48, Distance: 1.60km, Pace: 3:00/km";
          " -  -  Time: 00:06:10, Distance: 750m, Pace: 8:13/km";
          " -  -  -  Time: 00:00:14, Distance: 100m, Pace: 2:20/km";
          " -  -  -  Time: 00:01:00, Distance: 50m, Pace: 20:00/km";
          " -  -  -  Time: 00:00:14, Distance: 100m, Pace: 2:20/km";
          " -  -  -  Time: 00:01:00, Distance: 50m, Pace: 20:00/km";
          " -  -  -  Time: 00:00:14, Distance: 100m, Pace: 2:20/km";
          " -  -  -  Time: 00:01:00, Distance: 50m, Pace: 20:00/km";
          " -  -  -  Time: 00:00:14, Distance: 100m, Pace: 2:20/km";
          " -  -  -  Time: 00:01:00, Distance: 50m, Pace: 20:00/km";
          " -  -  -  Time: 00:00:14, Distance: 100m, Pace: 2:20/km";
          " -  -  -  Time: 00:01:00, Distance: 50m, Pace: 20:00/km";
          " -  -  Time: 00:07:00, Distance: 1.00km, Pace: 7:00/km";
          " -  Time: 00:02:00, Distance: 300m, Pace: 6:40/km" ]

    let intervals =
        parseToIntervalTree
            """2x(1km TR->FTS:200m + 3x(2min 5:00/km + 1.6km MAX) +
            5x(100m 2:20/km + 1min 50m) + 1km LE) + 2min 300m"""
        |> IntervalTree.toStringList

    intervals |> should equal expected
