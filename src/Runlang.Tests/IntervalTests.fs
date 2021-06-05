module IntervalTests

open FsUnit.Xunit
open Utils
open Xunit
open Interval
open Pace
open Distance
open Time

let replicateProgression paceValues =
    let createInterval (min, s) =
        let pace = Pace.create min s |> ok
        (Distance.kilometers 1m, pace) |> DistanceAndPace |> Interval.create

    paceValues |> List.map createInterval

[<Fact>]
let ``Interval created with distance and pace calculates the time`` () =
    let distance = Distance.kilometers 1.5m
    let pace = Pace.create 4 0 |> ok
    let interval = (distance, pace) |> DistanceAndPace |> Interval.create
    let expectedTime = Time.create 0 6 0 |> ok
    interval |> Interval.pace |> should equal pace
    interval |> Interval.distance |> should equal distance
    interval |> Interval.time |> should equal expectedTime


[<Fact>]
let ``Interval created with time and pace calculates the distance`` () =
    let time = Time.create 0 6 0 |> ok
    let pace = Pace.create 4 0 |> ok
    let interval = (time, pace) |> TimeAndPace |> Interval.create
    let expectedDistance = Distance.kilometers 1.5m
    interval |> Interval.pace |> should equal pace

    interval
    |> Interval.distance
    |> should equal expectedDistance

    interval |> Interval.time |> should equal time


[<Fact>]
let ``Interval created with distance and time calculates the pace`` () =
    let time = Time.create 0 6 0 |> ok
    let distance = Distance.kilometers 1.5m
    let interval = (time, distance) |> TimeAndDistance |> Interval.create
    let expectedPace = Pace.create 4 0 |> ok
    interval |> Interval.pace |> should equal expectedPace
    interval |> Interval.distance |> should equal distance
    interval |> Interval.time |> should equal time


[<Fact>]
let ``Positive progression should create intervals according to splits`` () =
    let first = Pace.create 5 0 |> ok
    let last = Pace.create 4 0 |> ok
    let distance = Distance.kilometers 7m
    let intervals = Interval.fromProgression distance first last

    let expectedPaceValues =
        [ (5, 0);
          (4, 50);
          (4, 40);
          (4, 30);
          (4, 20);
          (4, 10);
          (4, 0) ]

    let expectedIntervals = replicateProgression expectedPaceValues
    intervals |> should equal expectedIntervals


[<Fact>]
let ``Negative progression should create intervals according to splits`` () =
    let last = Pace.create 5 0 |> ok
    let first = Pace.create 4 0 |> ok
    let distance = Distance.kilometers 7m
    let intervals = Interval.fromProgression distance first last

    let expectedPaceValues =
        [ (5, 0);
          (4, 50);
          (4, 40);
          (4, 30);
          (4, 20);
          (4, 10);
          (4, 0) ]
        |> List.rev

    let expectedIntervals = replicateProgression expectedPaceValues
    intervals |> should equal expectedIntervals


[<Fact>]
let ``Progression with non integer km value uses last split as remainder`` () =
    let first = Pace.create 5 0 |> ok
    let last = Pace.create 4 0 |> ok
    let distance = Distance.kilometers 6.5m
    let intervals = Interval.fromProgression distance first last

    let fullKmPaceValues =
        [ (5, 0);
          (4, 50);
          (4, 40);
          (4, 30);
          (4, 20);
          (4, 10) ]

    let fullKmIntervals = replicateProgression fullKmPaceValues
    let pace = Pace.create 4 0 |> ok

    let expectedIntervals =
        (Distance.kilometers 0.5m, pace)
        |> DistanceAndPace
        |> Interval.create
        |> List.singleton
        |> List.append fullKmIntervals

    intervals |> should equal expectedIntervals


[<Fact>]
let ``Progression lower than 2km will yield two half distance intervals`` () =
    let first = Pace.create 5 0 |> ok
    let last = Pace.create 4 0 |> ok
    let distance = Distance.kilometers 1.5m
    let intervals = Interval.fromProgression distance first last
    let intervalDistance = Distance.meters 750

    let expectedIntervals =
        [ (intervalDistance, first); (intervalDistance, last) ]
        |> List.map (DistanceAndPace >> Interval.create)

    intervals |> should equal expectedIntervals


[<Fact>]
let ``To string must yield the correct string representation`` () =
    let pace = Pace.create 6 0 |> ok
    let distance = Distance.kilometers 1m
    let interval = (distance, pace) |> DistanceAndPace |> Interval.create
    let expected = "Time: 00:06:00, Distance: 1.00km, Pace: 6:00/km"
    interval |> Interval.toString |> should equal expected


[<Fact>]
let ``Summing intervals must yield the correct result`` () =
    let pace1 = Pace.create 5 0 |> ok
    let distance1 = Distance.kilometers 1m
    let interval1 = (distance1, pace1) |> DistanceAndPace |> Interval.create
    let time2 = Time.create 0 10 0 |> ok
    let distance2 = Distance.create 2.5m
    let interval2 = (time2, distance2) |> TimeAndDistance |> Interval.create
    let interval = Interval.sum interval1 interval2
    let expected = "Time: 00:15:00, Distance: 3.50km, Pace: 4:17/km"
    interval |> Interval.toString |> should equal expected


[<Fact>]
let ``Splitting interval by non divisible distance will yield extra``
    ()
    =
    let pace = Pace.create 4 30 |> ok
    let distance = Distance.kilometers 3.2m
    let splitSize = DistanceSplit (Distance.meters 500)
    let interval = Interval.create (DistanceAndPace (distance, pace))
    let splits = Interval.split splitSize interval

    let expected =
        [ "#1 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
          "#2 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
          "#3 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
          "#4 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
          "#5 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
          "#6 Time: 00:02:15, Distance: 500m, Pace: 4:30/km";
          "#7 Time: 00:00:54, Distance: 200m, Pace: 4:30/km" ]

    splits |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting by a divisible distance will yield exact quantity``
    ()
    =
    let pace = Pace.create 4 30 |> ok
    let distance = Distance.kilometers 3m
    let splitSize = DistanceSplit (Distance.kilometers 1m)
    let interval = Interval.create (DistanceAndPace (distance, pace))
    let splits = Interval.split splitSize interval

    let expected =
        [ "#1 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km";
          "#2 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km";
          "#3 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km" ]

    splits |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting interval by a bigger distance will yield the interval itself``
    ()
    =
    let pace = Pace.create 4 30 |> ok
    let distance = Distance.kilometers 1m
    let splitSize = DistanceSplit (Distance.kilometers 2.5m)
    let interval = Interval.create (DistanceAndPace (distance, pace))
    let splits = Interval.split splitSize interval
    let expected = [ "#1 Time: 00:04:30, Distance: 1.00km, Pace: 4:30/km" ]
    splits |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting by non divisible time will yield an extra interval``
    ()
    =
    let pace = Pace.create 4 0 |> ok
    let time = Time.create 0 25 0 |> ok
    let splitSize = TimeSplit (Time.fromMinutes 4m)
    let interval = Interval.create (TimeAndPace (time, pace))
    let splits = Interval.split splitSize interval

    let expected =
        [ "#1 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
          "#2 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
          "#3 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
          "#4 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
          "#5 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
          "#6 Time: 00:04:00, Distance: 1.00km, Pace: 4:00/km";
          "#7 Time: 00:01:00, Distance: 250m, Pace: 4:00/km" ]

    splits |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting by a divisible time will yield an exact quantity``
    ()
    =
    let pace = Pace.create 4 0 |> ok
    let time = Time.create 0 25 0 |> ok
    let splitSize = TimeSplit (Time.fromMinutes 5m)
    let interval = Interval.create (TimeAndPace (time, pace))
    let splits = Interval.split splitSize interval

    let expected =
        [ "#1 Time: 00:05:00, Distance: 1.25km, Pace: 4:00/km";
          "#2 Time: 00:05:00, Distance: 1.25km, Pace: 4:00/km";
          "#3 Time: 00:05:00, Distance: 1.25km, Pace: 4:00/km";
          "#4 Time: 00:05:00, Distance: 1.25km, Pace: 4:00/km";
          "#5 Time: 00:05:00, Distance: 1.25km, Pace: 4:00/km" ]

    splits |> Interval.listToString |> should equal expected


[<Fact>]
let ``Splitting interval by a bigger time will yield the interval itself`` () =
    let pace = Pace.create 4 25 |> ok
    let time = Time.create 0 4 25 |> ok
    let splitSize = TimeSplit (Time.fromMinutes 5m)
    let interval = Interval.create (TimeAndPace (time, pace))
    let splits = Interval.split splitSize interval
    let expected = [ "#1 Time: 00:04:25, Distance: 1.00km, Pace: 4:25/km" ]
    splits |> Interval.listToString |> should equal expected
