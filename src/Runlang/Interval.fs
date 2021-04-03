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

    let inline private getSplits count totalKm split =
        let getNextSplit splits _ =
            let remaining = totalKm - List.sum splits
            let next = if remaining >= split then split else remaining
            next::splits
        [2 .. count] |> List.fold getNextSplit [ split ] |> List.rev

    let fromProgression distance (TimePerKm first) (TimePerKm last) =
        let totalKm  = Distance.totalKm distance
        let count, split, dist =
            if totalKm >= 2.0m then
                int (ceil totalKm), 1m, Kilometers
            else 2, totalKm/2m, (*) 1000m >> uint >> Meters
        let firstMinutes = Time.totalMinutes first
        let ratio =
            (Time.totalMinutes last - firstMinutes) / (decimal (count - 1))
        let paces = applyProgression count firstMinutes ratio
        let distances = getSplits count totalKm split |> List.map dist
        List.zip distances paces |> List.map (DistanceAndPace >> create)

    let toString count {Time=time; Distance=dist; Pace=pace;} =
        let timeStr, distStr, paceStr =
            Time.toString time,
            Distance.toString dist,
            Pace.toString pace
        $"#{count} Time: {timeStr}, Distance: {distStr}, Pace: {paceStr}"

    let time interval = interval.Time
    let pace interval = interval.Pace
    let distance interval = interval.Distance


