module ListUtils

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
