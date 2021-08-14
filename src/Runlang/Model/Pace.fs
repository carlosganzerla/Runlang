module Pace

open Time
type Pace = TimePerKm of Time


[<RequireQualifiedAccess>]
module Pace =
    let create min s = Time.create 0 min s |> Result.map TimePerKm

    let value (TimePerKm pace) = pace

    let toString (TimePerKm pace) =
        let hours = Time.hours pace
        let hoursPart = if hours > 0 then sprintf "%d:" hours else ""
        sprintf "%s%d:%02d/km" hoursPart (Time.minutes pace) (Time.seconds pace)
