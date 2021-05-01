module Manipulation

open Interval
open RootList

type Manipulation = Interval list

type ManipulationList = RootList<Manipulation>

type OperationScope =
    | Index of int
    | Range of int * int
    | EntireList

[<RequireQualifiedAccess>]
module Manipulation =
    let private run f scope manipulation =
        let length = List.length manipulation
        let (a, b) =
            match scope with
            | Index x -> (x, x)
            | Range (a, b) -> (a, b)
            | EntireList -> (0, (length - 1))
        if a <= b && a >= 0 && b < length then
            Ok (f (a, b) manipulation length)
        else
            Error $"Invalid indexes {a}, {b}. Indexes go from 0 to {length}"

    let private joiner (a, b) (manipulation: Manipulation) length =
        let sum = manipulation.[a .. b] |> List.reduce Interval.sum
        manipulation.[0 .. a - 1] @
        [ sum ] @
        manipulation.[ b + 1 .. length - 1 ]

    let join = run joiner

    let private splitter split (a, b) (manipulation: Manipulation) length =
        let before =
            manipulation.[0 .. a - 1]
            |> List.map List.singleton
        let target =
            manipulation.[a .. b]
            |> List.map (Interval.split split)
        let after =
            manipulation.[b + 1 .. length - 1]
            |> List.map List.singleton
        before @ target @ after |> List.reduce (@)

    let split split = run (splitter split)

[<RequireQualifiedAccess>]
module ManipulationList = 
    let private getManipulation selected list =
        match selected with
        | Some x -> RootList.get x list
        | None -> RootList.top list |> Ok

    let join selected scope list =
        match getManipulation selected list with
        | Ok m -> 
            Manipulation.join scope m
            |> Result.map (RootList.add list)
        | Error err -> Error err
