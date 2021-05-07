// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FSharpx.State
open LangParser
open Pace
open Read

let paceTable = function
    | CL -> Pace.createOrThrow 08u 52u
    | CA -> Pace.createOrThrow 09u 51u
    | CV -> Pace.createOrThrow 11u 05u
    | TR -> Pace.createOrThrow 04u 47u
    | LVS -> Pace.createOrThrow 04u 31u
    | LE -> Pace.createOrThrow 04u 14u
    | MO -> Pace.createOrThrow 04u 01u
    | FO -> Pace.createOrThrow 03u 42u
    | FTS -> Pace.createOrThrow 03u 25u
    | MAX -> Pace.createOrThrow 02u 57u


[<EntryPoint>]
let rec main argv =
    let appM = app paceTable
    do eval appM New
    0 // return an integer exit code
