module Utils

let foldResult folder state list =
    let resultFolder result next = Result.bind (folder next) result
    List.fold resultFolder (Ok state) list

let execRange f rangeOpt list =
    let length = List.length list

    let (a, b) =
        match rangeOpt with
        | Some x -> x
        | None -> (0, length - 1)

    if a <= b && a >= 0 && b < length then
        Ok (f (a, b) list)
    else
        Error $"Invalid index"
