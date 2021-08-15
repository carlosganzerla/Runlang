module WorkoutPace

open Pace
open RunningTerm

type WorkoutPace =
    | Absolute of Pace
    | Term of RunningTerm

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


