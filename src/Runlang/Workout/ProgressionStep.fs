module ProgressionStep

open WorkoutPace
open Pace
open Interval
open Time
open Distance
open Utils
open EncodedWorkout

type ProgressionStep =
    { InitialPace: WorkoutPace;
      FinalPace: WorkoutPace;
      TotalDistance: Distance;
      SplitDistance: Distance }

[<RequireQualifiedAccess>]
module ProgressionStep =
    let create totalDistance initPace finalPace splitDistance =
        { TotalDistance = totalDistance;
          InitialPace = initPace;
          FinalPace = finalPace;
          SplitDistance = splitDistance }

    let toString step =
        let total = Distance.toString step.TotalDistance
        let initialPace = WorkoutPace.toString step.InitialPace
        let finalPace = WorkoutPace.toString step.FinalPace
        let split = Distance.toString step.SplitDistance
        $"{total} {initialPace}->{finalPace}:{split}"

    let private getSplits step =
        let totalDistance = Distance.totalKm step.TotalDistance
        let splitDistance = Distance.totalKm step.SplitDistance
        splitList totalDistance splitDistance

    let toIntervals paceTable step =
        let splits = getSplits step
        let splitCount = splits |> List.length |> decimal

        let toMinutes pace =
            pace
            |> WorkoutPace.toPace paceTable
            |> Pace.value
            |> Time.toMinutes

        let initialPace = toMinutes step.InitialPace
        let finalPace = toMinutes step.FinalPace
        let ratio = (finalPace - initialPace) / (splitCount - 1m)

        let getPace splitIdx _ =
            initialPace + (decimal (splitIdx - 1) * ratio)
            |> Time.fromMinutes
            |> TimePerKm

        let paces = splits |> List.mapi getPace

        let distances = splits |> List.map Distance.create

        List.zip distances paces
        |> List.map (DistanceAndPace >> Interval.create)

    let encode step =
        let splits = getSplits step
        let splitCount = List.length splits
        let initialPace = step.InitialPace
        let finalPace = step.FinalPace

        let duration split =
            split |> Distance.create |> Distance.totalMeters |> Distance

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
            |> EncodedWorkoutStep.createDefault (duration split) Active

        List.mapi createEncoding splits
