module WorkoutPace

open Pace
open EncodedWorkout
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

    let encodeIntensity =
        let fPace _ = Active

        let fTerm =
            function
            | CL
            | CA
            | CV -> Rest
            | TR
            | LVS
            | LE
            | MO -> Active
            | FO
            | FTS
            | MAX -> Interval

        map fPace fTerm
