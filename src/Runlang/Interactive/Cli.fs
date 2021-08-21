module InteractiveCli

open CommandParser
open LangParser
open PaceFileParser
open RootList
open WorkoutTree
open InteractiveExtensions
open ConsoleUtils
open Manipulation

exception PaceTableException of string

let parseArgs args =
    match Array.tryHead args with
    | Some paceFileName -> paceFileName
    | None -> ".pacetable"

let getPaceTable fileName =
    let contents = System.IO.File.ReadAllText fileName

    match parseTerms contents with
    | Ok termMap -> fun term -> Map.find term termMap
    | Error error -> raise (PaceTableException error)


let printList manipulations =
    do clear ()
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
        readMandatory "Enter workout string"
        |> parseWorkout
        |> Result.map (WorkoutTree.toIntervals table)
        |> Result.map RootList.create

    do printResult printList result

    match result with
    | Ok manipulations -> Updated manipulations
    | Error _ -> createState table

let rec updateState manipulations =
    let result = readMandatory "Enter Command" |> parseCommand manipulations

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

    do clear ()
    loop table New
