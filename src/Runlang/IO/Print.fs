module Print

open System
open Interval

let printIntervals = function
    | Ok intervals ->
        intervals
        |> Interval.listToString
        |> List.iter (printfn "%s")
    | Error err -> printfn "Failure: %s" err


let printManipulations = function
    | Ok intervals ->
        intervals
        |> Interval.listToString
        |> List.iter (printfn "%s")
    | Error err -> printfn "Failure: %s" err
