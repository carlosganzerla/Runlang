module IntervalTreeTests

open FsUnit.Xunit
open Utils
open Xunit
open IntervalTree


[<Fact>]
let ``To list with depth returns a list with depths`` () =
    let expected = [];

    let intervals =
        parseToIntervalTree
            "1km TR + 5x(400m MO + 400m LE + 100m FTS) + 1km LE"
        |> IntervalTree.toStringList

    intervals |> should equal expected

