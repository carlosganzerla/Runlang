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
