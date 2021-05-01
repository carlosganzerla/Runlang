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

type OperationScope =
    | Range of int * int
    | EntireList

let execScope f scope list =
    let length = List.length list
    let (a, b) =
        match scope with
        | Range (a, b) -> (a, b)
        | EntireList -> (0, (length - 1))
    if a <= b && a >= 0 && b < length then
        Ok (f (a,b) list)
    else
        Error $"Invalid indexes {a}, {b}. Indexes go from 0 to {length - 1}"
