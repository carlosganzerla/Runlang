module Read

open System
open FSharpx.State
open LangParser
open RootList
open Manipulation

type AppState =
    | New 
    | Updated of ManipulationList

let createState table = state {
    let parsed =
        printfn "Enter workout string:"
        |> Console.ReadLine
        |> parseWorkout table
    let newState =
        match parsed with
        | Ok intervals -> 
            let state = intervals |> RootList.create
            do state |> ManipulationList.toString |>  printfn "%s" 
            Updated state
        | Error err -> 
            do printfn "%s" err
            New
    do! putState newState
}

let updateState = state {
    do printf "Enter Command:"
    do printfn " NOT IMPLEMENTED YET"
    do! putState New
}

let app table = state {
    while true do
        let! state = getState
        do! match state with
            | Updated _ -> updateState
            | New -> createState table

}
