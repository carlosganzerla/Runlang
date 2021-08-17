module EncoderCli

open System
open Utils
open WorkoutTree
open EncodedWorkout
open LangParser

let rec test () =
    let result = Console.WriteLine "Enter Workout Mofo" |> Console.ReadLine |> parseWorkout

    match result with
    | Ok tree -> 
        let encoding = 
            Console.WriteLine $"Enter Workout Name: (unnamed)"
            |> Console.ReadLine
            |> EncodedWorkout.createEncoding 
        let steps = 
            tree
            |> WorkoutTree.encode
        let encoding = List.fold (flip EncodedWorkout.addStep) encoding steps
        Console.WriteLine $"Enter Workout File name: (test.fit)"
        |> Console.ReadLine
        |> EncodedWorkout.dumpFile encoding 
        |> test
    | Error err -> err |> Console.WriteLine |> test
