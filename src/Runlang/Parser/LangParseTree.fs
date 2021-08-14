module LangParseTree

open Distance
open Interval
open Pace
open Time
open RunningTerm
open Utils

type WorkoutPace =
    | Absolute of Pace
    | Term of RunningTerm

type ProgressionStep =
    { InitialPace: WorkoutPace;
      FinalPace: WorkoutPace;
      TotalDistance: Distance;
      SplitDistance: Distance }

type WorkoutStep =
    | DistanceAndPace of Distance * WorkoutPace
    | TimeAndPace of Time * WorkoutPace
    | TimeAndDistance of Time * Distance
    | Progression of ProgressionStep

type WorkoutTree =
    | Step of WorkoutStep
    | Repeat of uint * WorkoutTree list

[<RequireQualifiedAccess>]
module WorkoutPace =
    let toPace paceTable pace =
        match pace with
        | Absolute pace -> pace
        | Term term -> paceTable term

    let toString pace =
        match pace with
        | Absolute pace -> Pace.toString pace
        | Term term -> sprintf "%A" term

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
        |> List.map (Interval.DistanceAndPace >> Interval.create)

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
            

[<RequireQualifiedAccess>]
module WorkoutTree =
    let rec fold fRep fStep acc tree =
        let fold = fold fRep fStep

        match tree with
        | Repeat (count, repeats) -> List.fold fold (fRep acc count) repeats
        | Step step -> fStep acc step

    let rec foldBack fRep fStep tree acc =
        let fInt generator step = fun seed -> fStep step seed |> generator

        let fRep generator count = fun inner -> fRep count inner |> generator
        fold fRep fInt id tree acc

    let toIntervals paceTable tree =
        let fStep step list = step :: list

        let fRep count list =
            list |> List.replicate (int count) |> List.collect id

        foldBack fRep fStep tree []
        |> List.collect (WorkoutStep.toIntervals paceTable)

    let toFlatIntervals paceTable treeNodes =
        List.collect (toIntervals paceTable) treeNodes

    let rec toString nodes =
        // TODO: Make tail recursive
        let rec loop nodes acc =
            match nodes with
            | Repeat (count, nodes) :: tail ->
                let repeatString = $"{count}x({toString nodes})"
                loop tail (repeatString :: acc)
            | Step step :: tail ->
                let stepString = WorkoutStep.toString step
                loop tail (stepString :: acc)
            | [] -> List.rev acc

        System.String.Join (" + ", loop nodes [])
