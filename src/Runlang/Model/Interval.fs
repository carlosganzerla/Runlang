module Interval

open Distance
open Pace
open Time

type Interval = private {
    Distance: Distance
    Time: Time
    Pace: Pace
}

type IntervalType =
    | TimeAndPace of Time*Pace
    | TimeAndDistance of Time*Distance
    | DistanceAndPace of Distance*Pace

type IntervalSplit =
    | TimeSplit of Time
    | DistanceSplit of Distance

[<RequireQualifiedAccess>]
module Interval =
    let private timeInterval distance (pace:Pace) =
        let km = Distance.totalKm distance
        let minPerKm = pace |> Pace.value |> Time.totalMinutes
        let time = Time.totalTime (km * minPerKm)
        {Distance=distance; Time=time; Pace=pace}

    let private paceInterval time distance =
        let minutes = Time.totalMinutes time
        let km = Distance.totalKm distance
        let pace = Time.totalTime (minutes/ km) |> TimePerKm
        {Distance=distance; Time=time; Pace=pace}

    let private distanceInterval time pace =
        let timeMinutes = Time.totalMinutes time
        let paceMinutes = pace |> Pace.value |> Time.totalMinutes
        let distance = Distance.create (timeMinutes / paceMinutes)
        {Distance=distance; Time=time; Pace=pace}

    let create = function
        | TimeAndPace (time,pace) -> distanceInterval time pace
        | TimeAndDistance (time,dist)-> paceInterval time dist
        | DistanceAndPace (dist,pace) -> timeInterval dist pace

    let private applyProgression count firstMinutes ratio =
        let getPace idx =
            firstMinutes + (decimal (idx - 1) * ratio)
            |> Time.totalTime
            |> TimePerKm

        [1 .. count] |> List.map getPace

    let inline private getSplits (value: decimal) (splitSize: decimal) =
        let count = int (ceil (value / splitSize))

        let inline getNextSplit (splits, remaining) _ =
            if remaining >= splitSize then
                (splitSize::splits, remaining - splitSize)
            else
                (remaining::splits, remaining)

        [1 .. count]
        |> List.fold getNextSplit ([], value)
        |> fst
        |> List.rev

    let fromProgression distance (TimePerKm first) (TimePerKm last) =
        let totalKm  = Distance.totalKm distance

        let splitCount, splitSize, distFn =
            if totalKm >= 2.0m then
                int (ceil totalKm), 1m, Kilometers
            else
                2, totalKm/2m, (*) 1000m >> uint >> Meters

        let firstMinutes = Time.totalMinutes first

        let ratio =
            (Time.totalMinutes last - firstMinutes) / (decimal (splitCount - 1))

        let paces = applyProgression splitCount firstMinutes ratio
        let distances = getSplits totalKm splitSize |> List.map distFn
        List.zip distances paces |> List.map (DistanceAndPace >> create)

    let toString {Time=time; Distance=dist; Pace=pace;} =
        let timeStr, distStr, paceStr =
            Time.toString time,
            Distance.toString dist,
            Pace.toString pace
        $"Time: {timeStr}, Distance: {distStr}, Pace: {paceStr}"

    let listToString list =
        let withCount count interval  =
            sprintf "#%d %s" (count + 1) (toString interval)

        list |> List.mapi withCount

    let time interval = interval.Time
    let pace interval = interval.Pace
    let distance interval = interval.Distance

    let sum int1 int2 =
        let distance = Distance.sum int1.Distance int2.Distance
        let time = Time.sum int1.Time int2.Time
        create (TimeAndDistance (time, distance))

    let private splitByDistance distance interval =
        let splitSize = Distance.totalKm distance
        let totalKm = Distance.totalKm interval.Distance
        let splits = getSplits totalKm splitSize
        splits
        |> List.map Distance.create
        |> List.map (fun d -> d, interval.Pace)
        |> List.map DistanceAndPace
        |> List.map create

    let private splitByTime time interval =
        let splitSize = Time.totalMinutes time
        let totalMinutes = Time.totalMinutes interval.Time
        let splits = getSplits totalMinutes splitSize
        splits
        |> List.map Time.totalTime
        |> List.map (fun t -> t, interval.Pace)
        |> List.map TimeAndPace
        |> List.map create

    let split split interval =
        match split with
        | TimeSplit time -> splitByTime time interval
        | DistanceSplit dist -> splitByDistance dist interval
