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
        let fRep count intervals =
            intervals
            |> List.collect id
            |> List.replicate (int count)
            |> List.collect id
        catamorph fStep fRep tree

    let rec toString tree =
        let join (str: string list) = System.String.Join (" + ", str)
        let fStep = WorkoutStep.toString
        let fRep count steps = $"{count}x({join steps})"
        catamorph fStep fRep tree

    let encode tree =
        let fStep (step, index) =
            let steps = WorkoutStep.encode step
            (steps, index + List.length steps)
        let fRep count (stepsLists, index) = 
            let repeat = EncodedWorkoutStep.createRepeat index count
            let 
        catamorph fStep fRep tree |> fst
