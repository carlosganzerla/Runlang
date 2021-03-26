module ParserTests

open FsUnit.Xunit
open Xunit
open Models
open Parser

let paceTable = function
    | CL -> Pace.createOrThrow 03u 0u
    | CA -> Pace.createOrThrow 04u 0u
    | CV -> Pace.createOrThrow 05u 0u
    | TR -> Pace.createOrThrow 06u 0u
    | LVS -> Pace.createOrThrow 07u 0u
    | LE -> Pace.createOrThrow 08u 0u
    | MO -> Pace.createOrThrow 09u 0u
    | FO -> Pace.createOrThrow 10u 0u
    | FTS -> Pace.createOrThrow 11u 0u
    | MAX -> Pace.createOrThrow 12u 0u


let parse = parseWorkout paceTable

let isError = function
    | Error _ -> true
    | _ -> false


[<Fact>]
let ``Test shit`` () =
    Error "a" |> isError |> should equal true

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
