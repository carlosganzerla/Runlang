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

    let toListWithDepth tree =
        let rec toListWithDepth depth =
            function
            | Leaf interval -> [ (interval, depth) ]
            | Node (interval, subtrees) ->
                subtrees
                |> List.map (toListWithDepth (depth + 1))
                |> List.collect id
                |> flip (curry List.Cons)
                <| (interval, depth)

        toListWithDepth 0 tree

    let toStringList =
        let toString (interval, depth) =
            let dashes = " - " |> List.replicate depth |> System.String.Concat
            join " " [ dashes; Interval.toString interval ]

        toListWithDepth >> List.map toString

    let node subtrees =
        subtrees
        |> List.collect toList
        |> List.fold Interval.sum Interval.Zero
        |> (flip tuple) subtrees
        |> Node
