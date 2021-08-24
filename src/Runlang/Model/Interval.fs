module Interval

open Distance
open ListUtils
open Functions
open Pace
open Time


type Interval = private { Distance: Distance; Time: Time; Pace: Pace }

type IntervalType =
    | TimeAndPace of Time * Pace
    | TimeAndDistance of Time * Distance
    | DistanceAndPace of Distance * Pace

type IntervalSplit =
    | TimeSplit of Time
    | DistanceSplit of Distance

[<RequireQualifiedAccess>]
module Interval =
    let private timeInterval distance (pace: Pace) =
        let km = Distance.totalKm distance
        let minPerKm = pace |> Pace.time |> Time.toMinutes
        let time = (km * minPerKm) |> Time.fromMinutes
        { Distance = distance; Time = time; Pace = pace }

    let private paceInterval time distance =
        let minutes = Time.toMinutes time
        let km = Distance.totalKm distance
        let pace = (minutes / km) |> Time.fromMinutes |> TimePerKm
        { Distance = distance; Time = time; Pace = pace }

    let private distanceInterval time pace =
        let timeMinutes = Time.toMinutes time
        let paceMinutes = pace |> Pace.time |> Time.toMinutes
        let distance = Distance.create (timeMinutes / paceMinutes)
        { Distance = distance; Time = time; Pace = pace }

    let create =
        function
        | TimeAndPace (time, pace) -> distanceInterval time pace
        | TimeAndDistance (time, dist) -> paceInterval time dist
        | DistanceAndPace (dist, pace) -> timeInterval dist pace

    let toString { Time = time; Distance = dist; Pace = pace } =
        let timeStr, distStr, paceStr =
            Time.toString time, Distance.toString dist, Pace.toString pace

        $"Time: {timeStr}, Distance: {distStr}, Pace: {paceStr}"

    let listToString list =
        let withCount count interval =
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
        let splits = splitList totalKm splitSize

        let create =
            Distance.create
            >> (flip tuple) interval.Pace
            >> DistanceAndPace
            >> create

        List.map create splits

    let private splitByTime time interval =
        let splitSize = Time.toMinutes time
        let totalMinutes = Time.toMinutes interval.Time
        let splits = splitList totalMinutes splitSize

        let create =
            Time.fromMinutes
            >> (flip tuple) interval.Pace
            >> TimeAndPace
            >> create

        List.map create splits

    let split split interval =
        match split with
        | TimeSplit time -> splitByTime time interval
        | DistanceSplit dist -> splitByDistance dist interval
