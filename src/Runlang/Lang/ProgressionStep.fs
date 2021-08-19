module ProgressionStep

open WorkoutPace
open Pace
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

    let getSplits step =
        let totalDistance = Distance.totalKm step.TotalDistance
        let splitDistance = Distance.totalKm step.SplitDistance
        splitList totalDistance splitDistance |> List.map Distance.create

