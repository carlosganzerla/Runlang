module EncoderExtensions

open WorkoutPace
open Distance
open Time
open RunningTerm
open ProgressionStep
open StringUtils
open EncodedWorkout
open Functions

type StepEncodingMode =
    | Default
    | OpenDistance

[<RequireQualifiedAccess>]
module StepEncodingMode =
    let choice mode openVal defaultVal =
        match mode with
        | Default -> defaultVal
        | OpenDistance -> openVal

    let prefix mode value = choice mode value ""

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
    let encode mode step =
        let splits = ProgressionStep.getSplits step
        let splitCount = List.length splits
        let initialPace = step.InitialPace
        let finalPace = step.FinalPace

        let duration split =
            split
            |> Distance.totalMeters
            |> Distance
            |> StepEncodingMode.choice mode Open

        let name index split =
            let prefix =
                split |> Distance.toString |> StepEncodingMode.prefix mode

            let initial =
                if index = 0 then WorkoutPace.toString initialPace else ""

            let final = WorkoutPace.toString finalPace

            let dots =
                ('.', (splitCount - index) * 5 / splitCount)
                |> System.String

            join " " [ prefix; initial + dots + final ]

        let createEncoding index split =
            EncodedWorkoutStep.createDefault
            <| duration split
            <| Active
            <| name index split

        List.mapi createEncoding splits

[<RequireQualifiedAccess>]
module WorkoutStep =
    open WorkoutStep

    let encode mode step =
        let encode = EncodedWorkoutStep.createDefault

        let fDP distance pace =
            let duration =
                distance
                |> Distance.totalMeters
                |> Distance
                |> StepEncodingMode.choice mode Open

            let intensity = pace |> WorkoutPace.encodeIntensity

            let prefix =
                distance
                |> Distance.toString
                |> StepEncodingMode.prefix mode

            let name = join " " [ prefix; WorkoutPace.toString pace ]
            [ encode duration intensity name ]

        let fTP time pace =
            let duration = time |> Time.toSeconds |> Time
            let intensity = pace |> WorkoutPace.encodeIntensity
            let name = WorkoutPace.toString pace
            [ encode duration intensity name ]

        let fTD time distance =
            let duration = time |> Time.toSeconds |> Time
            let intensity = Active
            let name = Distance.toString distance
            [ encode duration intensity name ]

        let fPro = ProgressionStep.encode mode

        WorkoutStep.map fDP fTP fTD fPro step

[<RequireQualifiedAccess>]
module WorkoutTree =
    open WorkoutTree

    let encode mode =
        let fStep step acc =
            step
            |> WorkoutStep.encode mode
            |> List.fold (flip <| curry List.Cons) acc

        let fSingle _ acc = acc

        let fRep count stepsBefore repeatLoop =
            let fromIndex = List.length stepsBefore
            let repeatStep = EncodedWorkoutStep.createRepeat fromIndex count
            repeatStep :: repeatLoop

        WorkoutTree.loop fStep fSingle fRep [] >> List.rev
