module TimeTests

open FsUnit.Xunit
open Utils
open Xunit
open Time

[<Fact>]
let ``Minutes and seconds must be base sixty`` () =
     Time.create 0u 65u 30u |> shouldBeError
     Time.create 0u 30u 70u |> shouldBeError
     Time.create 0u 65u 90u |> shouldBeError

[<Fact>]
let ``Time must be craeted correcty`` () =
     let result = Time.create 2u 25u 30u
     let (Ok time) = result
     time |> Time.hours |> should equal 2u
     time |> Time.minutes |> should equal 25u
     time |> Time.seconds |> should equal 30u


[<Fact>]
let ``Total minutes must yield the time total minutes correctly`` () =
     let (Ok time) = Time.create 1u 30u 30u
     time |> Time.totalMinutes |> should equal 90.5m

[<Fact>]
let ``Total time should parse the time correctly from minutes`` () =
     let time = Time.totalTime 90.5m
     time |> Time.hours |> should equal 1u
     time |> Time.minutes |> should equal 30u
     time |> Time.seconds |> should equal 30u

[<Fact>]
let ``To string must return the correct representation`` () =
     let (Ok time) = Time.create 1u 30u 30u
     time |> Time.toString |> should equal "01:30:30"


[<Fact>]
let ``Summing time must yield the correct result`` () =
     let (Ok time1) = Time.create 1u 30u 30u
     let (Ok time2) = Time.create 2u 45u 39u
     let time = Time.sum time1 time2
     time |> Time.toString |> should equal "04:16:09"
