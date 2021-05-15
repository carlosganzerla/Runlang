module Repetition

open Interval

type Repetition =
    | Interval of Interval
    | RepList of Repetition list
    | RepCount of uint * Repetition

[<RequireQualifiedAccess>]
module Repetition =
    let toList repetition =
        let rec toList acc =
            function
            | RepCount (count, rep) ->
                rep |> List.replicate (int count) |> RepList |> (toList acc)
            | RepList reps -> reps |> List.fold toList acc
            | Interval int -> int :: acc

        toList [] repetition |> List.rev
