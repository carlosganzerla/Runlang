module ProgressionStep

open WorkoutPace
open Pace
open Interval
open Time
open Distance
open Utils

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

    let toIntervals paceTable step =
        let totalDistance = Distance.totalKm step.TotalDistance
        let splitDistance = Distance.totalKm step.SplitDistance

        let toMinutes pace =
            pace
            |> WorkoutPace.toPace paceTable
            |> Pace.value
            |> Time.toMinutes

        let splitCount = ceil totalDistance / splitDistance
        let initialPace = toMinutes step.InitialPace
        let finalPace = toMinutes step.FinalPace
        let ratio = (finalPace - initialPace) / (splitCount - 1m)

        let getPace splitIdx =
            initialPace + (decimal (splitIdx - 1) * ratio)
            |> Time.fromMinutes
            |> TimePerKm

        let paces = [ 1 .. int splitCount ] |> List.map getPace

        let distances =
            splitList totalDistance splitDistance
            |> List.map Distance.create

        List.zip distances paces
        |> List.map (DistanceAndPace >> Interval.create)
