module ParserTests

open FsUnit.Xunit
open Xunit
open Models
open Parser

let paceTable = function
    | MAX -> Pace.createOrThrow 03u 0u
    | FTS -> Pace.createOrThrow 04u 0u
    | FO -> Pace.createOrThrow 05u 0u
    | MO -> Pace.createOrThrow 06u 0u
    | LE -> Pace.createOrThrow 07u 0u
    | LVS -> Pace.createOrThrow 08u 0u
    | TR -> Pace.createOrThrow 09u 0u
    | CV -> Pace.createOrThrow 10u 0u
    | CA -> Pace.createOrThrow 11u 0u
    | CL -> Pace.createOrThrow 12u 0u

let parse = parseWorkout paceTable

let parseOk = parseWorkout paceTable >> function
    | Ok intervals -> List.mapi Interval.toString intervals
    | Error err-> raise (System.Exception err)

let parseErr = parseWorkout paceTable >> function
    | Error _-> ()
    | Ok _-> raise (System.Exception "An exception should have been thrown")


[<Fact>]
let ``Distance and term pace interval in km`` () =
    let input = "1km MO"
    let expected = ["#0 Time: 00:06:00, Distance: 1.00km, Pace: 00:06:00/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Distance and time pace interval in km`` () =
    let input = "1km 4:40/km"
    let expected = ["#0 Time: 00:04:40, Distance: 1.00km, Pace: 00:04:40/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Distance and time pace interval in m`` () =
    let input = "500m 4:40/km"
    let expected = ["#0 Time: 00:02:20, Distance: 500m, Pace: 00:04:40/km"]
    input |> parseOk |> should equal expected

[<Fact>]
let ``Numeric time and pace only seconds`` () =
    let input = "30s 2:30/km"
    let expected = ["#0 Time: 00:00:30, Distance: 200m, Pace: 00:02:30/km"]
    input |> parseOk |> should equal expected

// [<Fact>]
// let ``` () =
//     () |> should equal ()

// [<Fact>]
// let ``` () =
//     () |> should equal ()

// [<Fact>]
// let ``` () =
//     () |> should equal ()

// [<Fact>]
// let ``` () =
//     () |> should equal ()


// [<Fact>]
// let ``` () =
//     () |> should equal ()


// [<Fact>]
// let ``` () =
//     () |> should equal ()


// [<Fact>]
// let ``` () =
//     () |> should equal ()

// [<Fact>]
// let ``` () =
//     () |> should equal ()

// [<Fact>]
// let ``` () =
//     () |> should equal ()

// [<Fact>]
// let ``` () =
//     () |> should equal ()
