module Manipulation

open Interval
open RootList

type Manipulation = Interval list

type ManipulationList = RootList<Manipulation>

type ManipulationScope =
    | Range of int * int
    | EntireList

[<RequireQualifiedAccess>]
module Manipulation =
    let exec manipulation (a, b) =
        let length = List.length manipulation
        if a <= b && a >= 0 && b < length then
            let sum = manipulation.[a .. b] |> List.reduce Interval.sum
            let result =
                manipulation.[0 .. a - 1] @
                [ sum ] @
                manipulation.[ b + 1 .. length - 1 ]
            Ok result
        else
            Error $"Invalid indexes {a}, {b}. Indexes go from 0 to {length}"

    let join manipulation scope =
        let range =
            match scope with
            | Range (a, b) -> (a, b)
            | EntireList -> (0, (List.length manipulation) - 1)

        exec manipulation range
