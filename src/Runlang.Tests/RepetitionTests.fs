module RepetitionTests

open FsUnit.Xunit
open Utils
open Xunit
open Interval
open Pace
open Distance
open Repetition

let distance = Distance.kilometers 1.5m
let pace = Pace.create 4 0 |> ok
let interval = (distance, pace) |> DistanceAndPace |> Interval.create

let repetition =
    RepCount (
        5u,
        [ Interval interval;
          Interval interval;
          RepCount (
              2u,
              [ Interval interval; Interval interval; Interval interval ]
          ) ]
    )

[<Fact>]
let ``Repetition to yields a interval list from a recursive fold`` () =
    let expected = interval |> List.replicate 40
    repetition |> Repetition.toList |> should equal expected

[<Fact>]
let ``Fold should fold by left`` () =
    let fInt acc _ = acc + 1u
    let fRep count acc = acc * count
    let fold = Repetition.fold fRep fInt 0u repetition
    fold |> should equal 7u // Incorrect result, but expected

[<Fact>]
let ``Fold back should fold by right`` () =
    let fInt _ acc = acc + 1u
    let fRep acc count = acc * count
    let foldBack = Repetition.foldBack fRep fInt repetition 0u
    foldBack |> should equal 40u // Correct Result
