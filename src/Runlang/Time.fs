module Time

type Time = private {
    Hours: uint
    Minutes: uint
    Seconds: uint
}

module Time =
    let totalMinutes {Hours=h; Minutes=min; Seconds=s} =
       decimal h*60m + decimal min + (decimal s)/60m

    let totalTime (mins: decimal) =
        let seconds = (mins - floor mins)*60m |> round |> uint
        let minutes = (uint mins) % 60u + seconds / 60u
        let hours = uint mins / 60u + minutes / 60u
        {Hours=hours; Minutes=minutes % 60u; Seconds=seconds % 60u}

    let create h min s =
        if min > 59u || s > 59u then
            Error $"Invalid minutes and seconds: {min}, {s}"
        else
            Ok {Hours=h; Minutes=min; Seconds=s;}

    let toString {Hours=h; Minutes=min; Seconds=s} =
        sprintf "%02d:%02d:%02d" h min s

    let minutes time = time.Minutes
    let hours time = time.Hours
    let seconds time = time.Seconds

    let sum t1 t2 = 
        let minutesSum = (totalMinutes t1) + (totalMinutes t2)
        totalTime minutesSum 
