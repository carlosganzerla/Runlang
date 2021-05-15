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
        manipulation.[..a - 1] @ [ sum ] @ manipulation.[b + 1..]

    let join = execRange joiner

    let private splitter split (a, b) (manipulation: Manipulation) =
        let before = manipulation.[..a - 1] |> List.map List.singleton
        let target = manipulation.[a..b] |> List.map (Interval.split split)
        let after = manipulation.[b + 1..] |> List.map List.singleton
        before @ target @ after |> List.reduce (@)

    let split split = execRange (splitter split)

[<RequireQualifiedAccess>]
module ManipulationList =

    let toString manipulations =
        let count = RootList.length manipulations

        let header = sprintf "******%s(%d)******\n"

        let createString manipulation alias count =
            let cons h t = h :: t
            let header = header alias count

            manipulation
            |> Interval.listToString
            |> List.map (sprintf "%s\n")
            |> cons header
            |> String.concat ""

        let rec loop count acc =
            function
            | Root m -> (createString m "ROOT" count) :: acc
            | Cons (m, tail) ->
                let str = (createString m "MANIPULATION" count)
                loop (count - 1) (str :: acc) tail

        loop (count - 1) [] manipulations |> String.concat ""
