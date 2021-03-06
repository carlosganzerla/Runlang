module WorkoutTree

open WorkoutStep
open StringUtils

type WorkoutTree =
    private
    | Step of WorkoutStep
    | Repeat of uint * WorkoutTree list

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WorkoutTree =
    let step = Step

    let repeat count nodes =
        match (count, nodes) with
        | (0u, _)
        | (_, []) -> Error "Workout tree cannot be empty"
        | (1u, [ node ]) -> Ok node
        | subtree -> Ok <| Repeat subtree

    let rec loop fStep fSingle fRep acc tree =
        let loop = loop fStep fSingle fRep

        match tree with
        | Step step -> fStep step acc
        | Repeat (1u, nodes) -> nodes |> List.fold loop acc |> fSingle acc
        | Repeat (count, nodes) -> nodes |> List.fold loop acc |> fRep count acc

    let rec catamorph fStep fSingle fRep tree =
        let recurse = catamorph fStep fSingle fRep

        match tree with
        | Repeat (1u, nodes) -> nodes |> List.map recurse |> fSingle
        | Repeat (count, nodes) -> nodes |> List.map recurse |> fRep count
        | Step step -> fStep step

    let toString tree =
        let fStep = WorkoutStep.toString
        let fSingle = join " + "
        let fRep count steps = $"{count}x({fSingle steps})"
        catamorph fStep fSingle fRep tree
