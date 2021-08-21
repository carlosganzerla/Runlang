module EncoderCli

open System
open LangParser
open EncoderExtensions
open EncoderIo
open EncodedWorkout
open Functions

let trim (str: string) = str.Trim ()

let read () = () |> Console.ReadLine |> trim

let rec readMandatory desc =
    printfn "%s: " desc
    |> read
    |> function
        | "" -> readMandatory desc
        | value -> value

let readOptional desc fallback =
    printfn "%s (%s): " desc fallback
    |> read
    |> function
        | "" -> fallback
        | value -> value

let downloadWorkout tree =
    let steps = WorkoutTree.encode tree
    let name = readMandatory "Enter workout name"

    let encoding =
        EncodedWorkout.createEncoding name
        |> List.fold (flip EncodedWorkout.addStep)
        <| steps

    readOptional "Enter file path" $"/home/carlo/Documents/{name}.fit"
    |> EncodedWorkout.dumpFile encoding


let rec app () =
    readMandatory "Enter workout string"
    |> parseWorkout
    |> function
        | Ok tree -> downloadWorkout tree
        | Error error -> printfn "%s" error
    |> app
