module WorkoutTree

open WorkoutStep

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

    let rec toString tree =
        let fStep = WorkoutStep.toString
        let fEmpty () = ""
        let fSingle (steps: string list) = System.String.Join (" + ", steps)
        let fRep count steps = $"{count}x({fSingle steps})"
        catamorph fStep fEmpty fSingle fRep tree
