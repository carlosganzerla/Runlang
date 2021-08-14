module LangParseTree

open Pace
open Time
open Distance

type RunningTerm =
    | CL
    | CA
    | CV
    | TR
    | LVS
    | LE
    | MO
    | FO
    | FTS
    | MAX

type PaceTable = RunningTerm -> Pace

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
    | Progression of ProgressionStep

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
    let toIntervals paceTable step = ()

[<RequireQualifiedAccess>]
module WorkoutTree =
     let rec fold fRep fInt acc tree =
         let fold = fold fRep fInt

         match tree with
         | Repeat (count, rep) -> List.fold fold (fRep acc count) rep
         | Step interval -> fInt acc interval

     let rec foldBack fRep fInt repetition acc =
         let fInt generator interval =
             fun seed -> fInt interval seed |> generator

         let fRep generator count = fun inner -> fRep count inner |> generator
         fold fRep fInt id repetition acc

     // let toIntervalList paceTable tree =
     //     let fInt interval list = interval :: list

     //     let fRep count list =
     //         list |> List.replicate (int count) |> List.collect id

     //     foldBack fRep fInt repetition []


     // let flat repetitions = List.collect toList repetitions

