module IntervalTests

open FsUnit.Xunit
open Utils
open Xunit
open Interval
open Pace
open Distance
open Time

let replicateProgression paceValues =
    let createInterval (min,s) =
        let (Ok pace) = Pace.create min s
        (Kilometers 1m, pace)
        |> DistanceAndPace
        |> Interval.create
    paceValues |> List.map createInterval

[<Fact>]
let ``Interval created with distance and pace calculates the time`` () =
    let distance = Kilometers 1.5m
    let (Ok pace) = Pace.create 4u 0u
    let interval = (distance, pace) |> DistanceAndPace |> Interval.create
    let (Ok expectedTime) = Time.create 0u 6u 0u
    interval |> Interval.pace |> should equal pace
    interval |> Interval.distance |> should equal distance
    interval |> Interval.time |> should equal expectedTime


[<Fact>]
let ``Interval created with time and pace calculates the distance`` () =
    let (Ok time) = Time.create 0u 6u 0u
    let (Ok pace) = Pace.create 4u 0u
    let interval = (time, pace) |> TimeAndPace |> Interval.create
    let expectedDistance = Kilometers 1.5m
    interval |> Interval.pace |> should equal pace
    interval |> Interval.distance |> should equal expectedDistance
    interval |> Interval.time |> should equal time


[<Fact>]
let ``Interval created with distance and time calculates the pace`` () =
    let (Ok time) = Time.create 0u 6u 0u
    let distance = Kilometers 1.5m
    let interval = (time, distance) |> TimeAndDistance |> Interval.create
    let (Ok expectedPace) = Pace.create 4u 0u
    interval |> Interval.pace |> should equal expectedPace
    interval |> Interval.distance |> should equal distance
    interval |> Interval.time |> should equal time


[<Fact>]
let ``Positive progression should create intervals according to splits`` () =
    let (Ok first) = Pace.create 5u 0u
    let (Ok last) = Pace.create 4u 0u
    let distance = Kilometers 7m
    let intervals = Interval.fromProgression distance first last
    let expectedPaceValues = [
        (5u,0u); (4u, 50u); (4u, 40u); (4u, 30u); (4u, 20u); (4u, 10u);
        (4u, 0u);
    ]
    let expectedIntervals = replicateProgression expectedPaceValues
    intervals |> should equal expectedIntervals


[<Fact>]
let ``Negative progression should create intervals according to splits`` () =
    let (Ok last) = Pace.create 5u 0u
    let (Ok first) = Pace.create 4u 0u
    let distance = Kilometers 7m
    let intervals = Interval.fromProgression distance first last
    let expectedPaceValues =
        [
            (5u,0u); (4u, 50u); (4u, 40u); (4u, 30u); (4u, 20u); (4u, 10u);
            (4u, 0u);
        ] |> List.rev
    let expectedIntervals = replicateProgression expectedPaceValues
    intervals |> should equal expectedIntervals


[<Fact>]
let ``Progression with non integer km value uses last split as remainder`` () =
    let (Ok first) = Pace.create 5u 0u
    let (Ok last) = Pace.create 4u 0u
    let distance = Kilometers 6.5m
    let intervals = Interval.fromProgression distance first last
    let fullKmPaceValues =
        [
            (5u,0u); (4u, 50u); (4u, 40u); (4u, 30u); (4u, 20u); (4u, 10u);
        ]
    let fullKmIntervals = replicateProgression fullKmPaceValues
    let (Ok pace) = Pace.create 4u 0u
    let expectedIntervals =
        (Kilometers 0.5m, pace)
        |> DistanceAndPace
        |> Interval.create
        |> List.singleton
        |> List.append fullKmIntervals
    intervals |> should equal expectedIntervals


[<Fact>]
let ``Progression lower than 2km will yield two half distance intervals`` () =
    let (Ok first) = Pace.create 5u 0u
    let (Ok last) = Pace.create 4u 0u
    let distance = Kilometers 1.5m
    let intervals = Interval.fromProgression distance first last
    let intervalDistance = Meters 750u
    let expectedIntervals =
        [
            (intervalDistance, first); (intervalDistance, last);
        ] |> List.map (DistanceAndPace >> Interval.create)
    intervals |> should equal expectedIntervals


[<Fact>]
let ``To string must yield the correct string representation`` () =
    let (Ok pace) = Pace.create 6u 0u
    let distance = Kilometers 1m
    let interval = (distance, pace) |> DistanceAndPace |> Interval.create
    let expected = "Time: 00:06:00, Distance: 1.00km, Pace: 6:00/km"
    interval |> Interval.toString |> should equal expected


[<Fact>]
let ``Summing intervals must yield the correct result`` () =
    let (Ok pace1) = Pace.create 5u 0u
    let distance1 = Kilometers 1m
    let interval1 = (distance1, pace1) |> DistanceAndPace |> Interval.create
    let (Ok time2) = Time.create 0u 10u 0u
    let distance2 = Distance.create 2.5m
    let interval2 =  (time2, distance2) |> TimeAndDistance |> Interval.create
    let interval = Interval.sum interval1 interval2
    let expected = "Time: 00:15:00, Distance: 3.50km, Pace: 4:17/km"
    interval |> Interval.toString |> should equal expected


[<Fact>]
let ``Splitting interval by non divisible distance will yield an extra interval`` () =
    let (Ok pace) = Pace.create 4u 30u
    let distance = Kilometers 3.2m
    let splitSize = DistanceSplit (Meters 500u)
    let interval = Interval.create (DistanceAndPace (distance, pace))
    let splits = Interval.split splitSize interval 
    let expected = [
        "#1 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
        "#2 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
        "#3 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
        "#4 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
        "#5 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
        "#6 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
        "#7 Time: 00:00:54, Distance: 200m, Pace: 4:30/km";
    ]
    splits |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting interval by a divisible distance will yield the exact quantity of intervals`` () =
    let (Ok pace) = Pace.create 4u 30u
    let distance = Kilometers 3m
    let splitSize = DistanceSplit (Kilometers 1m)
    let interval = Interval.create (DistanceAndPace (distance, pace))
    let splits = Interval.split splitSize interval
    let expected = [
        "#1 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km";
        "#2 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km";
        "#3 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km";
    ]
    splits |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting interval by a bigger distance will yield the interval itself`` () =
    let (Ok pace) = Pace.create 4u 30u
    let distance = Kilometers 1m
    let splitSize = DistanceSplit (Kilometers 2.5m)
    let interval = Interval.create (DistanceAndPace (distance, pace))
    let splits = Interval.split splitSize interval 
    let expected = [
        "#1 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km";
    ]
    splits |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting interval by non divisible time will yield an extra interval`` () =
    let (Ok pace) = Pace.create 4u 0u
    let (Ok time) = Time.create 0u 25u 0u
    let splitSize = TimeSplit (Time.totalTime 4m)
    let interval = Interval.create (TimeAndPace (time, pace))
    let splits = Interval.split splitSize interval
    let expected = [
        "#1 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#2 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#3 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#4 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#5 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#6 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
        "#7 Time: 00:01:00, Distance: 250m, Pace: 4:00/km";
    ]
    splits |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting interval by a divisible time will yield the exact quantity of intervals`` () =
    let (Ok pace) = Pace.create 4u 0u
    let (Ok time) = Time.create 0u 25u 0u
    let splitSize = TimeSplit (Time.totalTime 5m)
    let interval = Interval.create (TimeAndPace (time, pace))
    let splits = Interval.split splitSize interval 
    let expected = [
        "#1 Time: 00:05:00, Distance: 1.25km, Pace: 4:00/km";
        "#2 Time: 00:05:00, Distance: 1.25km, Pace: 4:00/km";
        "#3 Time: 00:05:00, Distance: 1.25km, Pace: 4:00/km";
        "#4 Time: 00:05:00, Distance: 1.25km, Pace: 4:00/km";
        "#5 Time: 00:05:00, Distance: 1.25km, Pace: 4:00/km";
    ]
    splits |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting interval by a bigger time will yield the interval itself`` () =
    let (Ok pace) = Pace.create 4u 25u
    let (Ok time) = Time.create 0u 4u 25u
    let splitSize = TimeSplit (Time.totalTime 5m)
    let interval = Interval.create (TimeAndPace (time, pace))
    let splits = Interval.split splitSize interval 
    let expected = [
        "#1 Time: 00:04:25, Distance: 1.00km, Pace: 4:25/km";
    ]
    splits |> Interval.listToString |> should equal expected
