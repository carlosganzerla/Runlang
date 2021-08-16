module EncoderCli

open System
open WorkoutTree
open LangParser

let rec test () =
    let workout = Console.WriteLine "Enter Workout Mofo" |> Console.ReadLine
    let result = parseWorkout workout

    match result with
    | Ok tree -> tree |> WorkoutTree.encode |> printfn "%A" |> test
    | Error err -> err |> Console.WriteLine |> test
