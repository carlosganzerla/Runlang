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

    let rec catamorph fLeaf fNode tree =
        let recurse = catamorph fLeaf fNode 

        match tree with
        | Node (interval, subtrees) -> subtrees |> List.map recurse |> fNode interval
        | Leaf interval -> fLeaf interval

    let toList =
        let fLeaf = curry List.Cons
        let fNode = flip k
        fold fLeaf fNode [] >> List.rev

    let leaf = Leaf

    let toListWithDepth =
        let rec toListWithDepth depth tree = 
            let fLeaf interval = [(interval, depth)]
            let fNode interval intervals = 
                intervals
                |> List.collect (toListWithDepth (depth + 1))
                                
            catamorph fLeaf fNode tree
        toListWithDepth 0 >> List.rev

    let toStringList =
        let toString (interval, depth) =
            let dashes = " - " |> List.replicate depth |> System.String.Concat
            join " " [ dashes; Interval.toString interval ]

        toListWithDepth >> List.map toString

    let node subtrees =
        let fLeaf = curry List.Cons
        let fNode = fLeaf
        let fold = fold fLeaf fNode

        subtrees
        |> List.fold fold []
        |> List.reduce Interval.sum
        |> (flip tuple) subtrees
        |> Node
