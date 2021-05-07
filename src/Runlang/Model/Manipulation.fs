module Manipulation

open Interval
open RootList
open Utils

type Manipulation = Interval list

type ManipulationList = RootList<Manipulation>

[<RequireQualifiedAccess>]
module Manipulation =
    let private joiner (a, b) (manipulation: Manipulation) =
        let sum = manipulation.[a..b] |> List.reduce Interval.sum
        manipulation.[..a - 1] @
        [ sum ] @
        manipulation.[b + 1..]

    let join = execRange joiner

    let private splitter split (a, b) (manipulation: Manipulation) =
        let before =
            manipulation.[..a - 1]
            |> List.map List.singleton
        let target =
            manipulation.[a..b]
            |> List.map (Interval.split split)
        let after =
            manipulation.[b + 1..]
            |> List.map List.singleton
        before @ target @ after |> List.reduce (@)

    let split split = execRange (splitter split)
