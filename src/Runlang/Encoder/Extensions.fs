module EncoderExtensions

open WorkoutPace
open Distance
open Time
open RunningTerm
open ProgressionStep
open EncodedWorkout
open Functions

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
            |> EncodedWorkoutStep.createDefault
                (Distance.totalMeters >> Distance <| split)
                Active

        List.mapi createEncoding splits

[<RequireQualifiedAccess>]
module WorkoutStep =
    open WorkoutStep

    let private encodeString =
        let fDP _ pace = WorkoutPace.toString pace
        let fTP _ pace = WorkoutPace.toString pace
        let fTD _ distance = Distance.toString distance
        let fPro _ = ""
        WorkoutStep.map fDP fTP fTD fPro

    let encode step =
        let createEncoded duration intensity =
            step
            |> encodeString
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

    let encode =
        let fStep step acc = 
            step
            |> WorkoutStep.encode
            |> List.fold (flip <| curry List.Cons) acc
        let fSingle = id 
        let fRep count acc = 
            let fromIndex = List.length acc
            let repeatStep = EncodedWorkoutStep.createRepeat fromIndex count
            repeatStep :: (List.rev acc)
        WorkoutTree.fold fStep fSingle fRep []
            
