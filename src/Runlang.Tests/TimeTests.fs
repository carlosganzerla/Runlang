module TimeTests

open FsUnit.Xunit
open Xunit
open Time

let shouldEqual expected actual = actual |> (=) expected |> should equal true

[<Fact>]
let ``Minutes and seconds must be base sixty`` () =
     let expected = Error "Invalid minutes and seconds: 65, 30"
     create 0u 65u 30u |> shouldEqual expected

[<Fact>]
let ``Time must be craeted correcty`` () =
     let result = create 2u 25u 30u
     let (Ok time) = result
     time |> hours |> shouldEqual 2u
     time |> minutes |> shouldEqual 25u
     time |> seconds |> shouldEqual 30u


[<Fact>]
let ``Total minutes must yield the time total minutes correctly`` () =
     let (Ok time) = create 1u 30u 30u
     time |> totalMinutes |> shouldEqual 90.5m

[<Fact>]
let ``Total time should parse the time correctly from minutes`` () =
     let time = totalTime 90.5m
     time |> hours |> shouldEqual 2u
     time |> minutes |> shouldEqual 25u
     time |> seconds |> shouldEqual 30u

[<Fact>]
let ``To string must return the correct representation`` () =
     let (Ok time) = create 1u 30u 30u
     time |> toString |> shouldEqual "1:30:30"
