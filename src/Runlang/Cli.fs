module Cli

open CommandParser
open System
open LangParser
open Utils
open PaceFileParser
open RootList
open Manipulation

let parseArgs args =
    match args |> Array.truncate 1 with
    | [| paceFileName |] -> paceFileName
    | _ -> ".pacetable"

let getPaceTable fileName =
    let contents = System.IO.File.ReadAllText fileName
    let termMap = parseTerms contents |> ok
    fun term -> Map.find term termMap

let printList manipulations =
    do Console.Clear ()
    manipulations |> ManipulationList.toString |> printf "%s"

let printState =
    function
    | Updated m -> printList m
    | New -> ()

let printResult fprint =
    function
    | Ok ok -> do fprint ok
    | Error err -> do printfn "%s" err

let rec createState table =
    let result =
        printfn "Enter workout string:"
        |> Console.ReadLine
        |> parseWorkout table
        |> Result.map RootList.create

    do printResult printList result

    match result with
    | Ok manipulations -> Updated manipulations
    | Error _ -> createState table

let rec updateState manipulations =
    let result =
        printf "Enter Command:"
        |> Console.ReadLine
        |> parseCommand manipulations

    do printResult printState result

    match result with
    | Ok state -> state
    | Error _ -> updateState manipulations

let app args =
    let table = args |> parseArgs |> getPaceTable

    let rec loop table =
        function
        | Updated m -> updateState m |> loop table
        | New -> createState table |> loop table

    do Console.Clear ()
    loop table New
