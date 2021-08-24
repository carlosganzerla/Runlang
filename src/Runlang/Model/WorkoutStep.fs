module WorkoutStep

open Distance
open ProgressionStep
open WorkoutPace
open Time

type WorkoutStep =
    | DistanceAndPace of Distance * WorkoutPace
    | TimeAndPace of Time * WorkoutPace
    | TimeAndDistance of Time * Distance
    | Progression of ProgressionStep

[<RequireQualifiedAccess>]
module WorkoutStep =
    let map fDP fTP fTD fPro step =
        match step with
        | DistanceAndPace (dist, pace) -> fDP dist pace
        | TimeAndPace (time, pace) -> fTP time pace
        | TimeAndDistance (time, dist) -> fTD time dist
        | Progression pro -> fPro pro

    let toString =
        let fDP dist pace =
            $"{Distance.toString dist} {WorkoutPace.toString pace}"

        let fTP time pace = $"{Time.toString time} {WorkoutPace.toString pace}"
        let fTD time dist = $"{Time.toString time} {Distance.toString dist}"
        let fPro = ProgressionStep.toString
        map fDP fTP fTD fPro
