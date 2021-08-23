module InteractiveExtensions

open Interval
open Time
open Pace
open WorkoutPace

[<RequireQualifiedAccess>]
module ProgressionStep =
    open ProgressionStep

    let toIntervals paceTable step =
        let splits = ProgressionStep.getSplits step
        let splitCount = splits |> List.length |> decimal

        let toMinutes pace =
            pace
            |> WorkoutPace.toPace paceTable
            |> Pace.time
            |> Time.toMinutes

        let initialPace = toMinutes step.InitialPace
        let finalPace = toMinutes step.FinalPace
        let ratio = (finalPace - initialPace) / (splitCount - 1m)

        let getPace splitIdx _ =
            splitIdx
            |> decimal
            |> (*) ratio
            |> (+) initialPace
            |> Time.fromMinutes
            |> TimePerKm

        let paces = splits |> List.mapi getPace

        List.zip splits paces
        |> List.map (DistanceAndPace >> Interval.create)

[<RequireQualifiedAccess>]
module WorkoutStep =
    open WorkoutStep

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
        WorkoutStep.map fDP fTP fTD fPro


[<RequireQualifiedAccess>]
module WorkoutTree =
    open WorkoutTree

    let toIntervals paceTable tree =
        let fStep = WorkoutStep.toIntervals paceTable
        let fSingle = List.collect id

        let fRep count intervals =
            intervals
            |> fSingle
            |> List.replicate (int count)
            |> fSingle

        WorkoutTree.catamorph fStep fSingle fRep tree
