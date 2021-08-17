module WorkoutTree

open WorkoutStep
open Utils
open EncodedWorkout

type WorkoutTree =
    | Step of WorkoutStep
    | Repeat of uint * WorkoutTree list

[<RequireQualifiedAccess>]
module WorkoutTree =

    let rec catamorph fStep fEmpty fSingle fRep tree =
        let recurse = catamorph fStep fEmpty fSingle fRep

        match tree with
        | Repeat (_, [])
        | Repeat (0u, _) -> fEmpty ()
        | Repeat (1u, nodes) -> List.map recurse nodes |> fSingle
        | Repeat (count, nodes) -> nodes |> List.map recurse |> fRep count
        | Step step -> fStep step

    let toIntervals paceTable tree =
        let fStep = WorkoutStep.toIntervals paceTable
        let fSingle = List.collect id
        let fEmpty () = []

        let fRep count intervals =
            intervals
            |> fSingle
            |> List.replicate (int count)
            |> List.collect id

        catamorph fStep fEmpty fSingle fRep tree

    let rec toString tree =
        let fStep = WorkoutStep.toString
        let fEmpty () = ""
        let fSingle (steps: string list) = System.String.Join (" + ", steps)
        let fRep count steps = $"{count}x({fSingle steps})"
        catamorph fStep fEmpty fSingle fRep tree

    let encode tree =
        let rec loop tree acc =
            match tree with
            | Repeat (_, []) 
            | Repeat (0u, _) -> acc
            | Repeat (1u, nodes) -> nodes |> List.fold (flip loop) acc
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
