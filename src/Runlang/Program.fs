// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Parser
open Pace

let paceTable = function
    | CL -> createOrThrow 08u 52u
    | CA -> createOrThrow 09u 51u
    | CV -> createOrThrow 11u 05u
    | TR -> createOrThrow 04u 47u
    | LVS -> createOrThrow 04u 31u
    | LE -> createOrThrow 04u 14u
    | MO -> createOrThrow 04u 01u
    | FO -> createOrThrow 03u 42u
    | FTS -> createOrThrow 03u 25u
    | MAX -> createOrThrow 02u 57u

let print = function
    | Ok intervals ->
        intervals
        |> List.mapi Interval.toString
        |> List.iter (printfn "%s")
    | Error err -> printfn "Failure: %s" err

[<EntryPoint>]
let rec main argv =
    Console.WriteLine "Repetition:"
    |> Console.ReadLine
    |> parseWorkout paceTable
    |> print
    main argv |> ignore
    0 // return an integer exit code
