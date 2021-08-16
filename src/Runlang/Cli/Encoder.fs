module EncoderCli

open System
open WorkoutTree
open LangParser

let rec test () =
    let workout = Console.ReadLine ()
    let tree = parseWorkout workout

    match tree with
    | Ok ok -> ()
    | Error err -> err |> Console.WriteLine |> test
