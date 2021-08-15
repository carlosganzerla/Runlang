module WorkoutTree

open WorkoutStep

type WorkoutTree =
    | Step of WorkoutStep
    | Repeat of uint * WorkoutTree list

[<RequireQualifiedAccess>]
module WorkoutTree =
    let rec fold fRep fStep acc tree =
        let fold = fold fRep fStep

        match tree with
        | Repeat (count, repeats) -> List.fold fold (fRep acc count) repeats
        | Step step -> fStep acc step

    let rec foldBack fRep fStep tree acc =
        let fInt generator step = fun seed -> fStep step seed |> generator

        let fRep generator count = fun inner -> fRep count inner |> generator
        fold fRep fInt id tree acc

    let toIntervals paceTable tree =
        let fStep step list = step :: list

        let fRep count list =
            list |> List.replicate (int count) |> List.collect id

        foldBack fRep fStep tree []
        |> List.collect (WorkoutStep.toIntervals paceTable)

    let toFlatIntervals paceTable treeNodes =
        List.collect (toIntervals paceTable) treeNodes

    let rec toString nodes =
        // TODO: Make tail recursive
        let rec loop nodes acc =
            match nodes with
            | Repeat (count, nodes) :: tail ->
                let repeatString = $"{count}x({toString nodes})"
                loop tail (repeatString :: acc)
            | Step step :: tail ->
                let stepString = WorkoutStep.toString step
                loop tail (stepString :: acc)
            | [] -> List.rev acc

        System.String.Join (" + ", loop nodes [])
