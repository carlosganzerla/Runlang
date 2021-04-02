module Time

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
        {Hours=hours; Minutes=minutes; Seconds=seconds}

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
