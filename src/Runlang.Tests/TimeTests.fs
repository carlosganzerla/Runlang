module TimeTests

open FsUnit.Xunit
open Utils
open Xunit
open Time

[<Fact>]
let ``Minutes and seconds must be base sixty`` () =
    Time.create 0 65 30 |> shouldBeError
    Time.create 0 30 70 |> shouldBeError
    Time.create 0 65 90 |> shouldBeError

[<Fact>]
let ``Time must be craeted correcty`` () =
    let time = Time.create 2 25 30 |> ok
    time |> Time.hours |> should equal 2
    time |> Time.minutes |> should equal 25
    time |> Time.seconds |> should equal 30


[<Fact>]
let ``To minutes must yield the time total minutes correctly`` () =
    let time = Time.create 1 30 30 |> ok
    time |> Time.toMinutes |> should equal 90.5m

[<Fact>]
let ``From minutes should parse the time correctly from minutes`` () =
    let time = Time.fromMinutes 90.5m
    time |> Time.hours |> should equal 1
    time |> Time.minutes |> should equal 30
    time |> Time.seconds |> should equal 30

[<Fact>]
let ``To string must return the correct representation`` () =
    let time = Time.create 1 30 30 |> ok
    time |> Time.toString |> should equal "01:30:30"


[<Fact>]
let ``Summing time must yield the correct result`` () =
    let time1 = Time.create 1 30 30 |> ok
    let time2 = Time.create 2 45 39 |> ok
    let time = Time.sum time1 time2
    time |> Time.toString |> should equal "04:16:09"
