module Time

type Time = private { Hours: int; Minutes: int; Seconds: int }

[<RequireQualifiedAccess>]
module Time =
    let toMinutes { Hours = h; Minutes = min; Seconds = s } =
        decimal h * 60m + decimal min + (decimal s) / 60m

    let toSeconds { Hours = h; Minutes = min; Seconds = s } =
        h * 3600 + min * 60 + s

    let fromMinutes (mins: decimal) =
        let mins = abs mins
        let seconds = (mins - floor mins) * 60m |> round |> int
        let minutes = (int mins) % 60 + seconds / 60
        let hours = (int mins) / 60 + minutes / 60

        { Hours = hours;
          Minutes = minutes % 60;
          Seconds = seconds % 60 }

    let create h min s =
        let h = abs h
        let min = abs min
        let s = abs s

        if min > 59 || s > 59 then
            Error $"Invalid minutes and seconds: {min}, {s}"
        else
            Ok { Hours = h; Minutes = min; Seconds = s }

    let toString { Hours = h; Minutes = min; Seconds = s } =
        sprintf "%02d:%02d:%02d" h min s

    let minutes time = time.Minutes
    let hours time = time.Hours
    let seconds time = time.Seconds

    let sum t1 t2 =
        let minutesSum = (toMinutes t1) + (toMinutes t2)
        fromMinutes minutesSum
