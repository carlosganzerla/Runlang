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
            """2x(100m TR + 1min FTS + 2x(1km TR + 5x(20m MAX) +
        1min TR) + 1min CL) + 3x(1km FTS) + 10s 2:30/km"""
        |> IntervalTree.toListWithDepth

    intervals |> should equal expected

