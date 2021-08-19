module WorkoutPace

open Pace
open RunningTerm

type WorkoutPace =
    | Absolute of Pace
    | Term of RunningTerm

[<RequireQualifiedAccess>]
module WorkoutPace =
    let map fPace fTerm pace =
        match pace with
        | Absolute pace -> fPace pace
        | Term term -> fTerm term

    let toPace paceTable = map id paceTable

    let toString = map Pace.toString (sprintf "%A")

