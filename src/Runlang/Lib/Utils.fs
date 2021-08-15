module Utils

let foldResult folder state list =
    let resultFolder result next = Result.bind (folder next) result
    List.fold resultFolder (Ok state) list

let flip f x y = f y x

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

let splitList (value: decimal) (splitSize: decimal) =
    let count = int (ceil (value / splitSize))

    let getNextSplit (splits, remaining) _ =
        if remaining >= splitSize then
            (splitSize :: splits, remaining - splitSize)
        else
            (remaining :: splits, remaining)

    [ 1 .. count ]
    |> List.fold getNextSplit ([], value)
    |> fst
    |> List.rev
