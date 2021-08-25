module DistanceTests

open FsUnit.Xunit
open Xunit
open Distance

[<Fact>]
let ``Created distance should be in km if total km is not lower than one`` () =
    let distance = Distance.create 1.01m
    distance |> should equal (Distance.kilometers 1.01m)

[<Fact>]
let ``Created distance ignores sign`` () =
    let distance = Distance.create -3.4m
    distance |> should equal (Distance.kilometers 3.4m)

[<Fact>]
let ``Created distance should be in m if total km is lower than one`` () =
    let distance = Distance.create 0.5m
    distance |> should equal (Distance.meters 500)

[<Fact>]
let ``Total km should get the distance value in km`` () =
    Distance.create 0.5m
    |> Distance.totalKm
    |> should equal 0.5m

    Distance.create 1.5m
    |> Distance.totalKm
    |> should equal 1.5m

[<Fact>]
let ``Total meters should get the distance value in meters`` () =
    Distance.create 0.75m
    |> Distance.totalMeters
    |> should equal 750

    Distance.create 32.5326m
    |> Distance.totalMeters
    |> should equal 32533


[<Fact>]
let ``To string must yield correct representation`` () =
    Distance.meters 2500
    |> Distance.toString
    |> should equal "2500m"

    Distance.kilometers 1.854m
    |> Distance.toString
    |> should equal "1.85km"

[<Fact>]
let ``Summing distances must yield the correct result`` () =
    let d1 = Distance.create 2.55m
    let d2 = Distance.create 0.57m
    let sum1 = Distance.sum d1 d2
    let sum2 = Distance.sum d2 d1
    sum1 |> Distance.toString |> should equal "3.12km"
    sum2 |> Distance.toString |> should equal "3.12km"

[<Fact>]
let ``Distance.Zero is the identity element on sum`` () =
    let d1 = Distance.Zero
    let d2 = Distance.create 1.57m
    let sum1 = Distance.sum d1 d2
    let sum2 = Distance.sum d2 d1
    sum1 |> Distance.toString |> should equal "1.57km"
    sum2 |> Distance.toString |> should equal "1.57km"
