module ManipulationTests

open FsUnit.Xunit
open Utils
open Xunit
open LangParserTests
open Interval
open Manipulation
open Distance
open Time
open RootList

let intervals =
    parse "1km TR + 1km LVS + 1km LE + 1km MO + 1km FO + 1km FTS"

[<Fact>]
let ``Joining outside manipulation or with wrong indexing must yield error``
    ()
    =
    let joins =
        [ Manipulation.join (Some (-1, 5)) intervals;
          Manipulation.join (Some (1, 7)) intervals;
          Manipulation.join (Some (-1, 9)) intervals;
          Manipulation.join (Some (3, 2)) intervals ]

    joins |> List.iter shouldBeError

[<Fact>]
let ``Joining the entire manipulation should result in a single interval`` () =
    let join = Manipulation.join None intervals |> ok
    let expected = [ "#1 Time: 00:39:00, Distance: 6.00km, Pace: 6:30/km" ]
    join |> Interval.listToString |> should equal expected


[<Fact>]
let ``Joining by index or equal range is a no op`` () =
    let join = Manipulation.join (Some (2, 2)) intervals |> ok
    join |> should equal intervals


[<Fact>]
let ``Joining a range manipulation should covert it to a single interval`` () =
    let join = Manipulation.join (Some (1, 3)) intervals |> ok

    let expected =
        [ "#1 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          "#2 Time: 00:21:00, Distance: 3.00km, Pace: 7:00/km";
          "#3 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km";
          "#4 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km" ]

    join |> Interval.listToString |> should equal expected


[<Fact>]
let ``Joining empty list should be a no op`` () =
    let join = Manipulation.join (Some (0, 0)) []
    join |> shouldBeError

[<Fact>]
let ``Splitting outside manipulation or with wrong indexing must yield error``
    ()
    =
    let splitValue = Distance.meters 500 |> DistanceSplit

    let splits =
        [ Manipulation.split splitValue (Some (-1, 5)) intervals;
          Manipulation.split splitValue (Some (1, 7)) intervals;
          Manipulation.split splitValue (Some (-1, 9)) intervals;
          Manipulation.split splitValue (Some (3, 2)) intervals ]

    splits |> List.iter shouldBeError


[<Fact>]
let ``Splitting an index should split the interval accordingly`` () =
    let splitValue = Distance.meters 300 |> DistanceSplit
    let split = Manipulation.split splitValue (Some (1, 1)) intervals |> ok

    let expected =
        [ "#1 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          "#2 Time: 00:02:24, Distance: 300m, Pace: 8:00/km";
          "#3 Time: 00:02:24, Distance: 300m, Pace: 8:00/km";
          "#4 Time: 00:02:24, Distance: 300m, Pace: 8:00/km";
          "#5 Time: 00:00:48, Distance: 100m, Pace: 8:00/km";
          "#6 Time: 00:07:00, Distance: 1.00km, Pace: 7:00/km";
          "#7 Time: 00:06:00, Distance: 1.00km, Pace: 6:00/km";
          "#8 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km";
          "#9 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km" ]

    split |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting by range should split the intervals accordingly`` () =
    let time = Time.create 0 4 0 |> ok
    let splitValue = TimeSplit time
    let split = Manipulation.split splitValue (Some (1, 3)) intervals |> ok

    let expected =
        [ "#1 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
          "#2 Time: 00:04:00, Distance: 500m, Pace: 8:00/km";
          "#3 Time: 00:04:00, Distance: 500m, Pace: 8:00/km";
          "#4 Time: 00:04:00, Distance: 571m, Pace: 7:00/km";
          "#5 Time: 00:03:00, Distance: 429m, Pace: 7:00/km";
          "#6 Time: 00:04:00, Distance: 667m, Pace: 6:00/km";
          "#7 Time: 00:02:00, Distance: 333m, Pace: 6:00/km";
          "#8 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km";
          "#9 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km" ]

    split |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting entire list should split the intervals accordingly`` () =
    let splitValue = Distance.meters 500 |> DistanceSplit
    let split = Manipulation.split splitValue None intervals |> ok

    let expected =
        [ "#1 Time: 00:04:30, Distance: 500m, Pace: 9:00/km";
          "#2 Time: 00:04:30, Distance: 500m, Pace: 9:00/km";
          "#3 Time: 00:04:00, Distance: 500m, Pace: 8:00/km";
          "#4 Time: 00:04:00, Distance: 500m, Pace: 8:00/km";
          "#5 Time: 00:03:30, Distance: 500m, Pace: 7:00/km";
          "#6 Time: 00:03:30, Distance: 500m, Pace: 7:00/km";
          "#7 Time: 00:03:00, Distance: 500m, Pace: 6:00/km";
          "#8 Time: 00:03:00, Distance: 500m, Pace: 6:00/km";
          "#9 Time: 00:02:30, Distance: 500m, Pace: 5:00/km";
          "#10 Time: 00:02:30, Distance: 500m, Pace: 5:00/km";
          "#11 Time: 00:02:00, Distance: 500m, Pace: 4:00/km";
          "#12 Time: 00:02:00, Distance: 500m, Pace: 4:00/km" ]

    split |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting empty list should be a no op`` () =
    let splitValue = Distance.meters 500 |> DistanceSplit
    let split = Manipulation.split splitValue (Some (0, 0)) []
    split |> shouldBeError


[<Fact>]
let ``Manipulation List to string must yield the correct representation`` () =
    let distance = Distance.meters 500
    let time = Time.fromMinutes 2m
    let interval = (time, distance) |> TimeAndDistance |> Interval.create
    let manipulation = [ interval ]

    let manipulations : RootList<Manipulation> =
        List.replicate 5 manipulation |> RootList.fromList |> ok

    let expected = """******ROOT(0)******
#1 Time: 00:02:00, Distance: 500m, Pace: 4:00/km
******MANIPULATION(1)******
#1 Time: 00:02:00, Distance: 500m, Pace: 4:00/km
******MANIPULATION(2)******
#1 Time: 00:02:00, Distance: 500m, Pace: 4:00/km
******MANIPULATION(3)******
#1 Time: 00:02:00, Distance: 500m, Pace: 4:00/km
******MANIPULATION(4)******
#1 Time: 00:02:00, Distance: 500m, Pace: 4:00/km
"""
    let expectedLF = expected.Replace ("\r", "") // Windows workaround

    manipulations
    |> ManipulationList.toString
    |> should equal expectedLF
