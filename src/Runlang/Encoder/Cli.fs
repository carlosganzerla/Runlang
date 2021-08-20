module EncoderCli

open System
open LangParser
open EncoderExtensions
open EncodedWorkout
open Utils

let input () = Console.ReadLine().Trim() 

let rec readMandatory desc =
    printfn "%s: " desc
    |> input
    |> function
    | "" -> readMandatory desc
    | value -> value

let readOptional desc fallback = 
    printfn "%s (%s): " desc fallback 
    |> input
    |> function
    | "" -> fallback
    | value -> value

let createWorkout steps = 
    let encoding =
        readMandatory "Enter workout name:" 
        |> EncodedWorkout.createEncoding 
    List.fold 

let readWorkout () =  
    readMandatory "Enter workout string:"
    |> parseWorkout
    |> Result.map WorkoutTree.encode

