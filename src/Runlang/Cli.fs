module Cli

open CommandParser
open System
open LangParser
open RootList
open Manipulation

let createState table =
    printfn "Enter workout string:"
    |> Console.ReadLine
    |> parseWorkout table
    |> Result.map (RootList.create >> Updated)

let updateState manipulations =
    printf "Enter Command:"
    |> Console.ReadLine
    |> parseCommand manipulations

let getNextState table = function
    | Updated m -> 
        do m |> ManipulationList.toString |>  printfn "%s"
        updateState m
    | New -> createState table

let app table =
    let rec loop table prevState nextState =
        match nextState with
        | Ok current ->
            do if prevState <> current then Console.Clear ()
            current |> getNextState table |> loop table current
        | Error err ->
            do printfn "%s" err
            loop table prevState (Ok prevState)
    loop table New (Ok New)
