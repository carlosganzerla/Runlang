module LangParseTree

open Pace
open Interval
open Time
open RunningTerm
open Distance

type WorkoutPace =
    | Absolute of Pace
    | Term of RunningTerm

// type ProgressionStep = {
//     InitialPace: WorkoutPace;
//     FinalPace: WorkoutPace;
//     Distance: Distance;
//     Ratio: Distance;
// }

type WorkoutStep =
    | DistanceAndPace of Distance * WorkoutPace
    | TimeAndPace of Time * WorkoutPace
    | TimeAndDistance of Time * Distance

type WorkoutTree =
    | Step of WorkoutStep
    | Repeat of uint * WorkoutTree list

[<RequireQualifiedAccess>]
module WorkoutPace =
    let toPace paceTable pace =
        match pace with
        | Absolute pace -> pace
        | Term term -> paceTable term

    let toString pace =
        match pace with
        | Absolute pace -> Pace.toString pace
        | Term term -> sprintf "%A" term

// [<RequireQualifiedAccess>]
// module ProgressionStep =
//     let private inferProgression step =
//
//     let private applyProgression (count, initialPace, progressionRatio) =
//         let getPace idx =
//             initialPace + (decimal (idx - 1) * progressionRatio)
//             |> Time.fromMinutes
//             |> TimePerKm
//
//         [ 1 .. count ] |> List.map getPace
//
//
//     let toIntervals paceTable step =
//         let initialPace = step.InitialPace |> WorkoutPace.toPace paceTable |> Pace.value |> Time.toMinutes
//
//         let ratio =
//             (Time.toMinutes last - firstMinutes)
//             / (decimal (splitCount - 1))
//
//         let paces = applyProgression splitCount firstMinutes ratio
//
//         let distances = getSplits totalKm splitSize |> List.map distFn
//
//         List.zip distances paces
//         |> List.map (DistanceAndPace >> create)

[<RequireQualifiedAccess>]
module WorkoutStep =
    let toString step = 
        match step with
        | WorkoutStep.DistanceAndPace (distance, pace) ->
            $"{Distance.toString distance} {WorkoutPace.toString pace}"
        | WorkoutStep.TimeAndPace (time, pace) ->
            $"{Time.toString time} {WorkoutPace.toString pace}"
        | WorkoutStep.TimeAndDistance (time, distance) ->
            $"{Time.toString time} {Distance.toString distance}"

    let toIntervals paceTable step =
        match step with
        | WorkoutStep.DistanceAndPace (distance, pace) ->
            (distance, WorkoutPace.toPace paceTable pace)
            |> IntervalType.DistanceAndPace
            |> Interval.create
        | WorkoutStep.TimeAndPace (time, pace) ->
            (time, WorkoutPace.toPace paceTable pace)
            |> IntervalType.TimeAndPace
            |> Interval.create
        | WorkoutStep.TimeAndDistance (time, distance) ->
            (time, distance)
            |> IntervalType.TimeAndDistance
            |> Interval.create
        |> List.singleton

[<RequireQualifiedAccess>]
module WorkoutTree =
    let rec fold fRep fInt acc tree =
        let fold = fold fRep fInt

        match tree with
        | Repeat (count, rep) -> List.fold fold (fRep acc count) rep
        | Step interval -> fInt acc interval

    let rec foldBack fRep fInt tree acc =
        let fInt generator interval =
            fun seed -> fInt interval seed |> generator

        let fRep generator count = fun inner -> fRep count inner |> generator
        fold fRep fInt id tree acc


    let toIntervals paceTable tree =
        let fInt interval list = interval :: list

        let fRep count list =
            list |> List.replicate (int count) |> List.collect id

        foldBack fRep fInt tree []
        |> List.collect (WorkoutStep.toIntervals paceTable)

    let toFlatIntervals paceTable treeNodes = 
        List.collect (toIntervals paceTable) treeNodes

    let toString nodes = 
        let rec loop nodes acc = 
            match nodes with
            | Repeat (count, nodes)::tail -> 
                let repeatString = $"{count}x({loop nodes []})"
                loop tail (repeatString::acc)
            | Step step::tail -> 
                let stepString = WorkoutStep.toString step
                loop tail (stepString::acc)
            | [] -> List.rev acc
        System.String.Join (" + ", loop nodes [])

