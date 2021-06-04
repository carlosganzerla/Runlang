module Distance

open Udecimal

type Distance =
    | Meters of uint
    | Kilometers of Udecimal

[<RequireQualifiedAccess>]
module Distance =
    let create totalKm =
        if totalKm >= 1.0m then
            totalKm |> udecimal |> Kilometers
        else
            (totalKm * 1000m) |> round |> uint |> Meters

    let totalKm =
        function
        | Meters m -> (decimal m) / 1000m
        | Kilometers km -> sdecimal km

    let toString =
        function
        | Meters m -> sprintf "%im" m
        | Kilometers km -> sprintf "%.2fkm" (sdecimal km)

    let sum d1 d2 =
        let kmSum = (totalKm d1) + (totalKm d2)
        create kmSum
