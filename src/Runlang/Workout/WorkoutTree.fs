module WorkoutTree

open WorkoutStep
open Utils
open EncodedWorkout

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
        let rec loop tree acc =
            match tree with
            | Repeat (count, nodes) ->
                nodes
                |> flip (List.foldBack loop) []
                |> List.replicate (int count)
                |> List.collect id
                |> flip (List.foldBack <| curry List.Cons) acc
            | Step step ->
                step
                |> WorkoutStep.toIntervals paceTable
                |> flip (List.foldBack <| curry List.Cons) acc

        loop tree []

    let rec toString tree =
        // TODO: Make tail recursive
        let join (str: string list) = System.String.Join (" + ", str)

        let rec loop tree acc =
            match tree with
            | Repeat (count, nodes) ->
                let innerString =
                    nodes |> List.fold (flip loop) [] |> List.rev |> join

                let repeatString = $"{count}x({innerString})"
                repeatString :: acc
            | Step step ->
                let stepString = WorkoutStep.toString step
                stepString :: acc

        loop tree [] |> List.rev |> join


    let encode tree =
        // TODO: Make tail recursive / create catamorphism
        let rec loop tree acc =
            match tree with
            | Repeat (count, nodes) ->
                let fromIndex = List.length acc
                let stepsToRepeat = List.fold (flip loop) [] nodes
                let repeatStep = EncodedWorkoutStep.createRepeat fromIndex count
                repeatStep :: acc
            | Step step ->
                step
                |> WorkoutStep.encode
                |> List.fold (flip (curry List.Cons)) acc


        loop tree [] |> List.rev
