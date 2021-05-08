module Cli

open System
open LangParser
open RootList
open Manipulation

type AppState =
    | New
    | Updated of ManipulationList

let createState table =
    let parsed =
        printfn "Enter workout string:"
        |> Console.ReadLine
        |> parseWorkout table
    match parsed with
    | Ok intervals ->
        let state = intervals |> RootList.create
        do state |> ManipulationList.toString |>  printfn "%s"
        Updated state
    | Error err ->
        do printfn "%s" err
        New

let updateState m =
    do printf "Enter Command:"
    do printfn " NOT IMPLEMENTED YET"
    New

let rec app table = function
    | New -> table |> createState |> app table
    | Updated m -> m |> updateState |> app table
