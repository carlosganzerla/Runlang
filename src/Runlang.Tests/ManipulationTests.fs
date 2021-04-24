module ManipulationTests

open FsUnit.Xunit
open Utils
open Xunit
open ParserTests
open Interval
open Manipulation
open Distance
open Time

let intervals = parse "1km TR + 1km LVS + 1km LE + 1km MO + 1km FO + 1km FTS"

[<Fact>]
let ``Joining intervals outside manipulation or with wrong indexing must yield error`` () =
    let joins = [
        Manipulation.join (Range (-1,5)) intervals 
        Manipulation.join (Range (1,7)) intervals 
        Manipulation.join (Range (-1,9)) intervals 
        Manipulation.join (Range (3,2)) intervals 
    ]
    joins |> List.iter shouldBeError

[<Fact>]
let ``Joining the entire manipulation should result in a single interval`` () =
    let (Ok join) = Manipulation.join EntireList intervals 
    let expected = ["#1 Time: 00:39:00, Distance: 6.00km, Pace: 6:30/km"]
    join |> Interval.listToString |> should equal expected


[<Fact>]
let ``Joining by index or equal range is a no op`` () =
    let (Ok join1) = Manipulation.join (Index 5) intervals 
    let (Ok join2) = Manipulation.join (Range (2,2)) intervals 
    join1 |> should equal intervals
    join2 |> should equal intervals


[<Fact>]
let ``Joining a range manipulation should covert it to a single interval`` () =
    let (Ok join) = Manipulation.join (Range (1,3)) intervals 
    let expected = [
        "#1 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
        "#2 Time: 00:21:00, Distance: 3.00km, Pace: 7:00/km";
        "#3 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km";
        "#4 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
    ]
    join |> Interval.listToString |> should equal expected


[<Fact>]
let ``Joining empty list should be a no op`` () =
    let join = Manipulation.join EntireList []
    join |> shouldBeError

[<Fact>]
let ``Splitting intervals outside manipulation or with wrong indexing must yield error`` () =
    let splitValue = Meters 500u |> DistanceSplit
    let splits = [
        Manipulation.split splitValue (Range (-1,5)) intervals 
        Manipulation.split splitValue (Range (1,7)) intervals 
        Manipulation.split splitValue (Range (-1,9)) intervals 
        Manipulation.split splitValue (Range (3,2)) intervals 
    ]
    splits |> List.iter shouldBeError


[<Fact>]
let ``Splitting an index should split the interval accordingly`` () =
    let splitValue = Meters 300u |> DistanceSplit
    let (Ok split) = Manipulation.split splitValue (Index 1) intervals
    let expected = [
        "#1 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
        "#2 Time: 00:02:24, Distance: 300m, Pace: 8:00/km";
        "#3 Time: 00:02:24, Distance: 300m, Pace: 8:00/km";
        "#4 Time: 00:02:24, Distance: 300m, Pace: 8:00/km";
        "#5 Time: 00:00:48, Distance: 100m, Pace: 8:00/km";
        "#6 Time: 00:07:00, Distance: 1.00km, Pace: 7:00/km";
        "#7 Time: 00:06:00, Distance: 1.00km, Pace: 6:00/km";
        "#8 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km";
        "#9 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
    ]
    split |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting by range should split the intervals accordingly`` () =
    let (Ok time) = Time.create 0u 4u 0u
    let splitValue = TimeSplit time
    let (Ok split) = Manipulation.split splitValue (Range (1,3)) intervals
    let expected = [
        "#1 Time: 00:09:00, Distance: 1.00km, Pace: 9:00/km";
        "#2 Time: 00:04:00, Distance: 500m, Pace: 8:00/km";
        "#3 Time: 00:04:00, Distance: 500m, Pace: 8:00/km";
        "#4 Time: 00:04:00, Distance: 571m, Pace: 7:00/km";
        "#5 Time: 00:03:00, Distance: 429m, Pace: 7:00/km";
        "#6 Time: 00:04:00, Distance: 667m, Pace: 6:00/km";
        "#7 Time: 00:02:00, Distance: 333m, Pace: 6:00/km";
        "#8 Time: 00:05:00, Distance: 1.00km, Pace: 5:00/km";
        "#9 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
    ]
    split |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting entire list should split the intervals accordingly`` () =
    let splitValue = Meters 500u |> DistanceSplit
    let (Ok split) = Manipulation.split splitValue EntireList intervals
    let expected = [
        "#1 Time: 00:04:30, Distance: 500m, Pace: 9:00/km";
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
        "#12 Time: 00:02:00, Distance: 500m, Pace: 4:00/km";
    ]
    split |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting empty list should be a no op`` () =
    let splitValue = Meters 500u |> DistanceSplit
    let split = Manipulation.split splitValue EntireList []
    split |> shouldBeError