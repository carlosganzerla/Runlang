module RepetitionTests

open FsUnit.Xunit
open Utils
open Xunit
open Interval
open Pace
open Distance
open Repetition

[<Fact>]
let ``Repetition to yields a interval list from a recursive fold`` () =
    let distance = Kilometers 1.5m
    let (Ok pace) = Pace.create 4u 0u
    let interval = (distance, pace) |> DistanceAndPace |> Interval.create
    let repetition = 
        RepCount (5u, RepList [ 
            RepCount (2u, Interval interval);
            Interval interval;
            Interval interval;
            RepList [
                Interval interval;
                Interval interval;
            ];
        ]) 
    let expected = interval |> List.replicate 30 
    repetition |> Repetition.toList |> should equal expected
