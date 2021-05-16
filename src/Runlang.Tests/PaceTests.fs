module PaceTests

open FsUnit.Xunit
open Utils
open Xunit
open Time
open Pace

[<Fact>]
let ``Create pace arguments must be base sixty`` () =
    Pace.create 65u 30u |> shouldBeError
    Pace.create 3u 77u |> shouldBeError

[<Fact>]
let ``Pace must be craeted correcty`` () =
    let (TimePerKm pace) = Pace.create 5u 30u |> ok
    pace |> Time.hours |> should equal 0u
    pace |> Time.minutes |> should equal 5u
    pace |> Time.seconds |> should equal 30u


[<Fact>]
let ``To string must yield the correct pace representation`` () =
    let pace = Pace.create 5u 30u |> ok
    pace |> Pace.toString |> should equal "5:30/km"
