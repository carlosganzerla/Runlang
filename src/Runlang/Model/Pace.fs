module Pace

open Time

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

type Pace = TimePerKm of Time

type PaceTable = RunningTerm -> Pace

exception InvalidPaceException of string

[<RequireQualifiedAccess>]
module Pace =
    let create min s = Time.create 0u min s |> Result.map TimePerKm

    let value (TimePerKm pace) = pace

    let toString (TimePerKm pace) =
        sprintf "%d:%02d/km" (Time.minutes pace) (Time.seconds pace)

    let createOrThrow min s =
        create min s
        |> function
        | Ok ok -> ok
        | Error err -> raise (InvalidPaceException err)
