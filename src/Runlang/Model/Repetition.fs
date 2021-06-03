module Repetition

open Interval

type Repetition =
    | Interval of Interval
    | RepCount of uint * Repetition list

[<RequireQualifiedAccess>]
module Repetition =
    let rec fold fRep fInt acc repetition =
        let fold = fold fRep fInt

        match repetition with
        | RepCount (count, rep) -> List.fold fold (fRep acc count) rep
        | Interval interval -> fInt acc interval

    let rec foldBack fRep fInt repetition acc =
        let fInt generator interval =
            fun seed -> fInt interval seed |> generator

        let fRep generator count = fun inner -> fRep count inner |> generator
        fold fRep fInt id repetition acc

    let toList repetition =
        let fInt interval list = interval :: list

        let fRep count list =
            list |> List.replicate (int count) |> List.collect id

        foldBack fRep fInt repetition []

    let flat repetitions = List.collect toList repetitions
