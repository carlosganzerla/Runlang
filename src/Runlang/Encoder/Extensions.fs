module EncoderExtensions

open Utils
open WorkoutPace
open Distance
open Time
open RunningTerm
open ProgressionStep
open EncodedWorkout

[<RequireQualifiedAccess>]
module WorkoutPace = 
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

        WorkoutPace.map fPace fTerm

[<RequireQualifiedAccess>]
module ProgressionStep = 
    let encode step =
        let splits = ProgressionStep.getSplits step
        let splitCount = List.length splits
        let initialPace = step.InitialPace
        let finalPace = step.FinalPace

        let name index =
            let initialString =
                if index = 0 then WorkoutPace.toString initialPace else ""

            let finalString = WorkoutPace.toString finalPace

            let dots =
                ('.', (splitCount - index) * 5 / splitCount)
                |> System.String

            initialString + dots + finalString

        let createEncoding index split =
            index
            |> name
            |> EncodedWorkoutStep.createDefault (Distance.totalMeters >> Distance <| split) Active

        List.mapi createEncoding splits

[<RequireQualifiedAccess>]
module WorkoutStep = 
    open WorkoutStep

    let encode step =
        let createEncoded duration intensity =
            step
            |> WorkoutStep.toString
            |> EncodedWorkoutStep.createDefault duration intensity

        let fDP distance pace =
            let duration = distance |> Distance.totalMeters |> Distance
            let intensity = pace |> WorkoutPace.encodeIntensity
            [ createEncoded duration intensity ]

        let fTP time pace =
            let duration = time |> Time.toSeconds |> Time
            let intensity = pace |> WorkoutPace.encodeIntensity
            [ createEncoded duration intensity ]

        let fTD time _ =
            let duration = time |> Time.toSeconds |> Time
            let intensity = Active
            [ createEncoded duration intensity ]

        let fPro = ProgressionStep.encode

        WorkoutStep.map fDP fTP fTD fPro step

[<RequireQualifiedAccess>]
module WorkoutTree = 
    open WorkoutTree

    let encode tree =
        let rec loop tree acc =
            match tree with
            | WorkoutTree.Repeat (_, []) 
            | WorkoutTree.Repeat (0u, _) -> acc
            | WorkoutTree.Repeat (1u, nodes) -> nodes |> List.fold (flip loop) acc
            | WorkoutTree.Repeat (count, nodes) ->
                let fromIndex = List.length acc
                let repeatStep = EncodedWorkoutStep.createRepeat fromIndex count
                let repeatLoop = nodes |> List.fold (flip loop) acc
                repeatStep :: repeatLoop
            | WorkoutTree.Step step ->
                step
                |> WorkoutStep.encode
                |> List.fold (flip <| curry List.Cons) acc

        loop tree [] |> List.rev
