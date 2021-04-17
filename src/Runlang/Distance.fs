module Distance

type Distance =
    | Meters of uint
    | Kilometers of decimal

module Distance =
    let create totalKm =
        if totalKm >= 1.0m then
            Kilometers totalKm
        else
            (totalKm * 1000m) |> uint |> Meters

    let totalKm = function
        | Meters m -> (decimal m) / 1000m
        | Kilometers km -> km

    let toString = function
        | Meters m -> sprintf "%im" m
        | Kilometers km -> sprintf "%.2fkm" km

    let sum d1 d2 =
        let kmSum = (totalKm d1) + (totalKm d2)
        create kmSum
