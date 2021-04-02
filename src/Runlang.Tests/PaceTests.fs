module PaceTests

open FsUnit.Xunit
open Utils
open Xunit
open Pace

[<Fact>]
let ``Create pace arguments must be base sixty`` () =
     let expected : Result<Pace,string> =
          Error "Invalid minutes and seconds: 65, 30"
     create 65u 30u |> shouldBeError
     create 3u 77u |> shouldBeError

[<Fact>]
let ``Pace must be craeted correcty`` () =
     let (Ok (TimePerKm pace)) = create 5u 30u
     pace |> Time.hours |> should equal 0u
     pace |> Time.minutes |> should equal 5u
     pace |> Time.seconds |> should equal 30u


[<Fact>]
let ``To string must yield the correct pace representation`` () =
     let (Ok pace) = create 5u 30u
     pace |> toString |> should equal "5:30/km"
