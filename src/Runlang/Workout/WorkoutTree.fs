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
        let fStep step list = step :: list

        let fRep count list =
            list |> List.replicate (int count) |> List.collect id

        foldBack fRep fStep tree []
        |> List.collect (WorkoutStep.toIntervals paceTable)

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


    let encode tree encoding =
        // TODO: Make tail recursive / create catamorphism
        let rec loop tree (encoding, index) =
            match tree with
            | Repeat (count, nodes) ->
                let fromStep = index

                let (encoding, index) =
                    List.fold (flip loop) (encoding, index) nodes

                (EncodedWorkout.addRepeat fromStep count encoding, index)
            | Step step ->
                let steps = step |> WorkoutStep.encode

                let encoding =
                    List.fold (flip EncodedWorkout.addStep) encoding steps

                let offset = List.length steps
                (encoding, index + offset)

        loop tree (encoding, 0)
