module DistanceTests

open FsUnit.Xunit
open Xunit
open Distance

[<Fact>]
let ``Created distance should be in km if total km is not lower than 1`` () =
    let distance = create 1.01m
    distance |> should equal (Kilometers 1.01m)


[<Fact>]
let ``Created distance should be in m if total km is lower than 1`` () =
    let distance = create 0.5m
    distance |> should equal (Meters 500u)


[<Fact>]
let ``Total km should get the distance value in km`` () =
    create 0.5m |> totalKm |> should equal 0.5m
    create 1.5m |> totalKm |> should equal 1.5m


[<Fact>]
let ``To string must yield correct representation`` () =
    Meters 500u |> toString |> should equal "500m"
    Kilometers 1.854m |> toString |> should equal "1.85km"
