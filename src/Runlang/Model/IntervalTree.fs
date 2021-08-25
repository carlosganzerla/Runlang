module IntervalTree

open Interval
open Functions
open StringUtils

type IntervalTree =
    private
    | Leaf of Interval
    | Node of Interval * IntervalTree list

[<RequireQualifiedAccess>]
module IntervalTree =
    let rec fold fLeaf fNode acc tree =
        let fold = fold fLeaf fNode

        match tree with
        | Node (interval, subtrees) ->
            subtrees |> List.fold fold (fNode interval acc)
        | Leaf interval -> fLeaf interval acc

    let toList =
        let fLeaf = curry List.Cons
        let fNode = flip k
        fold fLeaf fNode [] >> List.rev

    let leaf = Leaf

    let rec toListWithDepth depth tree =
        match tree with
        | Leaf interval -> [(interval, depth)]
        | Node (interval, subtrees) -> 
            (interval, depth) :: (List.map (toListWithDepth (depth + 1)) subtrees |> List.collect id)

    let toStringList =
        let toString (interval, depth) =
            let dashes = " - " |> List.replicate depth |> System.String.Concat
            join " " [ dashes; Interval.toString interval ]

        toListWithDepth 0 >> List.map toString

    let node subtrees =
        let fLeaf = curry List.Cons
        let fNode = fLeaf
        let fold = fold fLeaf fNode

        subtrees
        |> List.fold fold []
        |> List.reduce Interval.sum
        |> (flip tuple) subtrees
        |> Node
