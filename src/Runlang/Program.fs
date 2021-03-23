// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Parser
open Models

let paceTableResult = function
    | CL -> Pace.create 08u 52u
    | CA -> Pace.create 09u 51u
    | CV -> Pace.create 11u 05u
    | TR -> Pace.create 04u 47u
    | LVS -> Pace.create 04u 31u
    | LE -> Pace.create 04u 14u
    | MO -> Pace.create 04u 01u
    | FO -> Pace.create 03u 42u
    | FTS -> Pace.create 03u 25u
    | MAX -> Pace.create 02u 57u

let paceTable pace =
    match paceTableResult pace with
    | Ok ok -> ok
    | Error err -> raise (InvalidPace err)

[<EntryPoint>]
let rec main argv =
    Console.WriteLine "Repetition:"
    |> Console.ReadLine
    |> run repetition paceTable
    main argv |> ignore
    0 // return an integer exit code
