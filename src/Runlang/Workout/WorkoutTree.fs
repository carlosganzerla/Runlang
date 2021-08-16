module WorkoutTree

open WorkoutStep
open Utils
open EncodedWorkout

// TODO Check if this can be optimized to better support TCO
type WorkoutTree =
    | Step of WorkoutStep
    | Repeat of uint * WorkoutTree list

[<RequireQualifiedAccess>]
module WorkoutTree =
    // TODO Optimize this MO'FO
    // TODO Make this tail recursive, or Create catamorphism if not possible.
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
        let join (str: string list) = System.String.Join (" + ", str)

        let rec loop tree acc =
            match tree with
            | Repeat (count, nodes) ->
                let innerString =
                    nodes |> flip (List.foldBack loop) [] |> List.rev |> join

                let repeatString = $"{count}x({innerString})"
                repeatString :: acc
            | Step step ->
                let stepString = WorkoutStep.toString step
                stepString :: acc

        loop tree [] |> List.rev |> join


    let encode tree =
        let rec loop tree acc =
            match tree with
            | Repeat (_, []) -> acc
            | Repeat (count, nodes) ->
                let fromIndex = List.length acc
                let repeatStep = EncodedWorkoutStep.createRepeat fromIndex count
                let repeatLoop = nodes |> List.fold (flip loop) acc
                repeatStep :: repeatLoop
            | Step step ->
                step
                |> WorkoutStep.encode
                |> List.fold (flip <| curry List.Cons) acc

        loop tree [] |> List.rev
