module Read

open System
open FSharpx.State
open LangParser
open RootList
open Manipulation

type AppState =
    | Quit
    | New of unit
    | Updated of ManipulationList

let createState table = state {
    let parsed =
        Console.WriteLine "Enter workout string:"
        |> Console.ReadLine
        |> parseWorkout table
    let newState =
        match parsed with
        | Ok intervals -> intervals |> RootList.create |> Updated
        | Error err -> err |> Console.WriteLine |> New
    do! putState newState
}

let updateState = state {
    do Console.WriteLine "Enter Command:"
    do Console.WriteLine "NOT IMPLEMENTED YET"
    let! state = getState
    let newState =
        match state with
        | Updated m -> Updated m
        | _ -> New ()
    do! putState newState
}

let run table = state {
    while getState () do

}
