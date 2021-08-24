module IntervalEncoder

open Interval
open IntervalTree
open Time
open Pace
open WorkoutPace

[<RequireQualifiedAccess>]
module ProgressionStep =
    open ProgressionStep

    let toIntervalTree paceTable step =
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
        |> List.map (DistanceAndPace >> Interval.create >> IntervalTree.leaf)
        |> IntervalTree.node


[<RequireQualifiedAccess>]
module WorkoutStep =
    open WorkoutStep

    let toIntervalTree paceTable =
        let fDP distance pace =
            (distance, WorkoutPace.toPace paceTable pace)
            |> IntervalType.DistanceAndPace
            |> Interval.create
            |> IntervalTree.leaf

        let fTP time pace =
            (time, WorkoutPace.toPace paceTable pace)
            |> IntervalType.TimeAndPace
            |> Interval.create
            |> IntervalTree.leaf

        let fTD time distance =
            (time, distance)
            |> IntervalType.TimeAndDistance
            |> Interval.create
            |> IntervalTree.leaf

        let fPro = ProgressionStep.toIntervalTree paceTable
        WorkoutStep.map fDP fTP fTD fPro


[<RequireQualifiedAccess>]
module WorkoutTree =
    open WorkoutTree

    let toIntervalTree paceTable tree =
        let fStep = WorkoutStep.toIntervalTree paceTable
        let fSingle = IntervalTree.node
        let fRep count subtrees = 
            subtrees
            |> List.replicate (int count)
            |> List.collect id
            |> IntervalTree.node

        WorkoutTree.catamorph fStep fSingle fRep tree
