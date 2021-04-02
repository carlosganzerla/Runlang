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
     let result = create 2u 25u 30u
     let (Ok time) = result
     time |> Time.hours |> should equal 2u
     time |> Time.minutes |> should equal 25u
     time |> Time.seconds |> should equal 30u


[<Fact>]
let ``Total minutes must yield the time total minutes correctly`` () =
     let (Ok time) = create 1u 30u 30u
     time |> Time.totalMinutes |> should equal 90.5m

[<Fact>]
let ``Total time should parse the time correctly from minutes`` () =
     let time = totalTime 90.5m
     time |> Time.hours |> should equal 1u
     time |> Time.minutes |> should equal 30u
     time |> Time.seconds |> should equal 30u

[<Fact>]
let ``To string must return the correct representation`` () =
     let (Ok time) = create 1u 30u 30u
     time |> Time.toString |> should equal "01:30:30"
