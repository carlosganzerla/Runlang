module PaceTests

open FsUnit.Xunit
open Utils
open Xunit
open Time
open Pace

[<Fact>]
let ``Create pace arguments must be base sixty`` () =
    Pace.create 65 30 |> shouldBeError
    Pace.create 3 77 |> shouldBeError

[<Fact>]
let ``Pace must be created correcty`` () =
    let pace = Pace.create 5 30 |> ok
    pace |> Pace.time |> Time.hours |> should equal 0
    pace |> Pace.time |> Time.minutes |> should equal 5
    pace |> Pace.time |> Time.seconds |> should equal 30

[<Fact>]
let ``To string must yield the correct pace representation`` () =
    let pace = Pace.create 5 30 |> ok
    pace |> Pace.toString |> should equal "5:30/km"

[<Fact>]
let ``To string must show hours if they are bigger than 0`` () =
    let pace = Time.create 3 12 40 |> ok |> TimePerKm
    pace |> Pace.toString |> should equal "3:12:40/km"
