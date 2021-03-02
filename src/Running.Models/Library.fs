module Types

type Time = private {
    Hours: int
    Minutes: int
    Seconds: int
}

type Pace = Time

type Distance =
    | Meters of int
    | Kilometers of decimal

type Interval = private {
    Distance: Distance
    Time: Time
    Pace: Pace
}

type IntervalType =
    | TimeAndPace of Time*Pace
    | TimeAndDistance of Time*Distance
    | DistanceAndPace of Distance*Pace

let createDistance totalKm =
    if totalKm >= 1.0m then
        Kilometers totalKm
    else
        (totalKm * 1000m) |> int |> Meters

let totalMinutes {Hours=h; Minutes=min; Seconds=s} =
   decimal h*60m + decimal min + (decimal s)/60m

let totalTime mins =
    let hours = int mins / 60
    let minutes = (int mins) % 60
    let seconds = int ((mins - truncate mins)*60m)
    { Hours=hours; Minutes=minutes; Seconds=seconds}

let private base60 num =
    if num >= 0 && num < 60 then
        Ok num
    else
        Error (sprintf "%i is not a base 60 number." num)

let createTime h min s =
    match base60 min, base60 s with
    | Ok min, Ok s -> Ok {Hours=h; Minutes=min; Seconds=s;}
    | Error e, _ ->  Error e
    | _, Error e -> Error e

let createPace min s: Result<Pace,string> = createTime 0 min s

let totalKm = function
    | Meters m -> (decimal m) / 1000m
    | Kilometers km -> km

let timeInterval distance (pace:Pace) =
    let km = totalKm distance
    let minPerKm = totalMinutes pace
    let time = totalTime (km * minPerKm)
    {Distance=distance; Time=time; Pace=pace}

let paceInterval time distance =
    let minutes = totalMinutes time
    let km = totalKm distance
    let pace = totalTime (minutes/ km)
    {Distance=distance; Time=time; Pace=pace}

let distanceInterval time (pace:Pace) =
    let timeMinutes = totalMinutes time
    let paceMinutes = totalMinutes pace
    let distance = createDistance (timeMinutes / paceMinutes)
    {Distance=distance; Time=time; Pace=pace}

let createInterval = function
    | TimeAndPace (time,pace) -> distanceInterval time pace
    | TimeAndDistance (time,dist)-> paceInterval time dist
    | DistanceAndPace (dist,pace) -> timeInterval dist pace
