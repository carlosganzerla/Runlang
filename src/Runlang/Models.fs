module Types

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
        { Hours=hours; Minutes=minutes; Seconds=seconds}

    let create h min s =
        let realS = s % 60u
        let extraMin = s / 60u
        let realMin = (min + extraMin) % 60u
        let extraH = (min + extraMin) / 60u
        let realH = h + extraH
        {Hours=realH; Minutes=realMin; Seconds=realS;}

type Pace = Time

module Pace =
    let create min s: Pace = Time.create 0u min s


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

module Interval =

    type Type =
        | TimeAndPace of Time*Pace
        | TimeAndDistance of Time*Distance
        | DistanceAndPace of Distance*Pace

    let private timeInterval distance (pace:Pace) =
        let km = Distance.totalKm distance
        let minPerKm = Time.totalMinutes pace
        let time = Time.totalTime (km * minPerKm)
        {Distance=distance; Time=time; Pace=pace}

    let private paceInterval time distance =
        let minutes = Time.totalMinutes time
        let km = Distance.totalKm distance
        let pace = Time.totalTime (minutes/ km)
        {Distance=distance; Time=time; Pace=pace}

    let private distanceInterval time (pace:Pace) =
        let timeMinutes = Time.totalMinutes time
        let paceMinutes = Time.totalMinutes pace
        let distance = Distance.create (timeMinutes / paceMinutes)
        {Distance=distance; Time=time; Pace=pace}

    let create = function
        | TimeAndPace (time,pace) -> distanceInterval time pace
        | TimeAndDistance (time,dist)-> paceInterval time dist
        | DistanceAndPace (dist,pace) -> timeInterval dist pace
