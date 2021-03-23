module Models

type Time = private {
    Hours: uint
    Minutes: uint
    Seconds: uint
}

module Time =
    let totalMinutes {Hours=h; Minutes=min; Seconds=s} =
       decimal h*60m + decimal min + (decimal s)/60m

    let totalTime mins =
        let hours = uint mins / 60u
        let minutes = (uint mins) % 60u
        let seconds = (mins - truncate mins)*60m |> round |> uint
        {Hours=hours; Minutes=minutes; Seconds=seconds}

    let create h min s =
        if min > 59u || s > 59u then
            Error $"Invalid minutes and seconds: {min}, {s}"
        else
            Ok {Hours=h; Minutes=min; Seconds=s;}

type RunningTerm =
    | CL
    | CA
    | CV
    | TR
    | LVS
    | LE
    | MO
    | FO
    | FTS
    | MAX

type Pace = TimePerKm of Time

type PaceTable = RunningTerm -> Pace

exception InvalidPace of string

module Pace =
    let create min s = Time.create 0u min s |> Result.map TimePerKm

    let value (TimePerKm pace) = pace

type Distance =
    | Meters of uint
    | Kilometers of decimal

module Distance =
    let create totalKm =
        if totalKm >= 1.0m then
            Kilometers totalKm
        else
            (totalKm* 1000m) |> uint |> Meters

    let totalKm = function
        | Meters m -> (decimal m) / 1000m
        | Kilometers km -> km

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

type Repetition =
    | Interval of Interval
    | RepList of Repetition list
    | RepCount of uint*Repetition

module Repetition =
    let toList repetition =
        let rec toList acc = function
            | RepCount (count, rep) ->
                rep
                |> List.replicate (int count)
                |> RepList
                |> (toList acc)
            | RepList reps ->  reps |> List.fold toList acc
            | Interval int -> int::acc
        toList [] repetition |> List.rev
