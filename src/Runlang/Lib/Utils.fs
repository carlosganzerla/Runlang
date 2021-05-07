module Utils

let fromResultList list =
    let folder list next =
        Result.bind (fun list ->
            Result.bind (fun x -> Ok (x::list)) next
        ) list
    List.fold folder (Ok []) list

let foldResult folder state list =
    let resultFolder result next =
        Result.bind (folder next) result
    List.fold resultFolder (Ok state) list

let execRange f (a, b) list =
    let length = List.length list
    if a <= b && a >= 0 && b < length then
        Ok (f (a, b) list)
    else
        Error $"Invalid indexes {a}, {b}. Indexes go from 0 to {length - 1}"
