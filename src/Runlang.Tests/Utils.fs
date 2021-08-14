module Utils

open System
open RunningTerm
open Pace
open LangParser
open LangParseTree

let shouldBeError =
    function
    | Error _ -> ()
    | _ -> raise (Exception ("Expected Error got Ok"))

let ok =
    function
    | Ok ok -> ok
    | Error err -> raise (Exception ($"{err}"))

let paceTable =
    function
    | MAX -> Pace.create 3 0
    | FTS -> Pace.create 4 0
    | FO -> Pace.create 5 0
    | MO -> Pace.create 6 0
    | LE -> Pace.create 7 0
    | LVS -> Pace.create 8 0
    | TR -> Pace.create 9 0
    | CV -> Pace.create 10 0
    | CA -> Pace.create 11 0
    | CL -> Pace.create 12 0
    >> ok

let parseToIntervals workout =
    parseWorkout workout
    |> ok
    |> WorkoutTree.toFlatIntervals paceTable
