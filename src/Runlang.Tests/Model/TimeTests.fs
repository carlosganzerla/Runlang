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
    Time.create 0 -55 -101 |> shouldBeError
    Time.create 0 -67 31 |> shouldBeError

[<Fact>]
let ``Time must be created correcty`` () =
    let time = Time.create 2 25 30 |> ok
    time |> Time.hours |> should equal 2
    time |> Time.minutes |> should equal 25
    time |> Time.seconds |> should equal 30

[<Fact>]
let ``Negative times have the sign ignored`` () =
    let time = Time.create -6 -32 -11 |> ok
    time |> Time.hours |> should equal 6
    time |> Time.minutes |> should equal 32
    time |> Time.seconds |> should equal 11

[<Fact>]
let ``To minutes must yield the time total minutes correctly`` () =
    let time = Time.create 1 30 30 |> ok
    time |> Time.toMinutes |> should equal 90.5m

[<Fact>]
let ``To minutes must yield the time total seconds correctly`` () =
    let time = Time.create 1 30 30 |> ok
    time |> Time.toSeconds |> should equal 5430

[<Fact>]
let ``From minutes should parse the time correctly from minutes`` () =
    let time = Time.fromMinutes 90.5m
    time |> Time.hours |> should equal 1
    time |> Time.minutes |> should equal 30
    time |> Time.seconds |> should equal 30

[<Fact>]
let ``From minutes ignores sign`` () =
    let time = Time.fromMinutes -25.4m
    time |> Time.hours |> should equal 0
    time |> Time.minutes |> should equal 25
    time |> Time.seconds |> should equal 24

[<Fact>]
let ``To string must return the correct representation`` () =
    let time = Time.create 1 30 30 |> ok
    time |> Time.toString |> should equal "01:30:30"

[<Fact>]
let ``Summing time must yield the correct result`` () =
    let time1 = Time.create 1 30 30 |> ok
    let time2 = Time.create 2 45 39 |> ok
    let sum1 = Time.sum time1 time2
    let sum2 = Time.sum time2 time1
    sum1 |> Time.toString |> should equal "04:16:09"
    sum2 |> Time.toString |> should equal "04:16:09"

[<Fact>]
let ``Time.Zero is the identity element on sum`` () =
    let time = Time.create 1 30 30 |> ok
    let sum1 = Time.sum time Time.Zero
    let sum2 = Time.sum Time.Zero time
    sum1 |> Time.toString |> should equal "01:30:30"
    sum2 |> Time.toString |> should equal "01:30:30"
