module Types

type Time = private {
    Hours: uint
    Minutes: uint
    Seconds: uint
}

type Pace = Time

type Distance =
    | Meters of uint
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
        (totalKm* 1000m) |> uint |> Meters

let totalMinutes {Hours=h; Minutes=min; Seconds=s} =
   decimal h*60m + decimal min + (decimal s)/60m

let totalTime mins =
    let hours = uint mins / 60u
    let minutes = (uint mins) % 60u
    let seconds = (mins - truncate mins)*60m |> round |> uint
    { Hours=hours; Minutes=minutes; Seconds=seconds}

let private check60 num =
    if num >= 0 && num < 60 then
        Ok num
    else
        Error (sprintf "%i must be betwenn 0 and 59." num)

let createTime h min s =
    let realS = s % 60u
    let extraMin = s / 60u
    let realMin = (min + extraMin) % 60u
    let extraH = (min + extraMin) / 60u
    let realH = h + extraH
    {Hours=realH; Minutes=realMin; Seconds=realS;}

let createPace min s: Pace = createTime 0u min s

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
