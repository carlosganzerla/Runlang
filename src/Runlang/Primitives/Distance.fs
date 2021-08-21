module Distance

type Distance =
    private
    | Meters of int
    | Kilometers of decimal

[<RequireQualifiedAccess>]
module Distance =
    let meters m = Meters (abs m)
    let kilometers km = Kilometers (abs km)

    let create totalKm =
        let totalKm = abs totalKm

        if totalKm >= 1.0m then
            totalKm |> Kilometers
        else
            (totalKm * 1000m) |> round |> int |> Meters

    let totalKm =
        function
        | Meters m -> (decimal m) / 1000m
        | Kilometers km -> km

    let totalMeters =
        function
        | Meters m -> m
        | Kilometers km -> int (km * 1000m)

    let toString =
        function
        | Meters m -> sprintf "%im" m
        | Kilometers km -> sprintf "%.2fkm" km

    let sum d1 d2 =
        let kmSum = (totalKm d1) + (totalKm d2)
        create kmSum
