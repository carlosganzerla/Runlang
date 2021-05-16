module DistanceTests

open FsUnit.Xunit
open Xunit
open Distance

[<Fact>]
let ``Created distance should be in km if total km is not lower than one`` () =
    let distance = Distance.create 1.01m
    distance |> should equal (Kilometers 1.01m)


[<Fact>]
let ``Created distance should be in m if total km is lower than one`` () =
    let distance = Distance.create 0.5m
    distance |> should equal (Meters 500u)


[<Fact>]
let ``Total km should get the distance value in km`` () =
    Distance.create 0.5m |> Distance.totalKm |> should equal 0.5m
    Distance.create 1.5m |> Distance.totalKm |> should equal 1.5m


[<Fact>]
let ``To string must yield correct representation`` () =
    Meters 500u |> Distance.toString |> should equal "500m"
    Kilometers 1.854m |> Distance.toString |> should equal "1.85km"
    

[<Fact>]
let ``Summing distances must yield the correct result`` () =
    let d1 = Distance.create 2.55m
    let d2 = Distance.create 0.57m
    let distance = Distance.sum d1 d2
    distance |> Distance.toString |> should equal "3.12km"
