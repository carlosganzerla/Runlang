module WorkoutStep

open Distance
open ProgressionStep
open EncodedWorkout
open WorkoutPace
open Interval
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
        | WorkoutStep.DistanceAndPace (dist, pace) -> fDP dist pace
        | WorkoutStep.TimeAndPace (time, pace) -> fTP time pace
        | WorkoutStep.TimeAndDistance (time, dist) -> fTD time dist
        | WorkoutStep.Progression pro -> fPro pro

    let toString =
        let fDP dist pace =
            $"{Distance.toString dist} {WorkoutPace.toString pace}"

        let fTP time pace = $"{Time.toString time} {WorkoutPace.toString pace}"
        let fTD time dist = $"{Time.toString time} {Distance.toString dist}"
        let fPro = ProgressionStep.toString
        map fDP fTP fTD fPro

    let toIntervals paceTable =
        let fDP distance pace =
            (distance, WorkoutPace.toPace paceTable pace)
            |> IntervalType.DistanceAndPace
            |> Interval.create
            |> List.singleton

        let fTP time pace =
            (time, WorkoutPace.toPace paceTable pace)
            |> IntervalType.TimeAndPace
            |> Interval.create
            |> List.singleton

        let fTD time distance =
            (time, distance)
            |> IntervalType.TimeAndDistance
            |> Interval.create
            |> List.singleton

        let fPro = ProgressionStep.toIntervals paceTable
        map fDP fTP fTD fPro

    let encode step =
        let createEncoded duration intensity =
            step
            |> toString
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

        map fDP fTP fTD fPro step
