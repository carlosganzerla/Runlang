module WorkoutStep

open Distance
open ProgressionStep
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
    let toString step =
        match step with
        | WorkoutStep.DistanceAndPace (distance, pace) ->
            $"{Distance.toString distance} {WorkoutPace.toString pace}"
        | WorkoutStep.TimeAndPace (time, pace) ->
            $"{Time.toString time} {WorkoutPace.toString pace}"
        | WorkoutStep.TimeAndDistance (time, distance) ->
            $"{Time.toString time} {Distance.toString distance}"
        | WorkoutStep.Progression progression ->
            ProgressionStep.toString progression

    let toIntervals paceTable step =
        match step with
        | WorkoutStep.DistanceAndPace (distance, pace) ->
            (distance, WorkoutPace.toPace paceTable pace)
            |> IntervalType.DistanceAndPace
            |> Interval.create
            |> List.singleton
        | WorkoutStep.TimeAndPace (time, pace) ->
            (time, WorkoutPace.toPace paceTable pace)
            |> IntervalType.TimeAndPace
            |> Interval.create
            |> List.singleton
        | WorkoutStep.TimeAndDistance (time, distance) ->
            (time, distance)
            |> IntervalType.TimeAndDistance
            |> Interval.create
            |> List.singleton
        | WorkoutStep.Progression progression ->
            ProgressionStep.toIntervals paceTable progression

