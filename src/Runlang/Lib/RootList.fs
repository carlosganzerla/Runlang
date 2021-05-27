module RootList

open Utils

type RootList<'T> =
    | Root of 'T
    | Cons of 'T * RootList<'T>

[<RequireQualifiedAccess>]
module RootList =
    let create = Root

    let add list e = Cons (e, list)

    let rec fold folder acc list =
        let loop = fold folder
        match list with
        | Cons (e, list) -> (folder (acc, e), list) ||> loop
        | Root r -> folder (acc, r)

    let length list = fold (fst >> (+) 1) 0 list

    let root list = fold snd list

    let top =
        function
        | Cons (e, _) -> e
        | Root r -> r

    let toList list = fold (fun (es ,e) -> e :: es) [] list

    let fromList =
        function
        | [] -> Error "List cannot be empty!"
        | list -> list.[1..] |> List.fold add (create list.[0]) |> Ok

    let get idx list =
        let length = length list
        let seed = Error "Invalid index"
        let folder ((state, currentIdx), e) =
            match state with
            | Ok _ -> (state, currentIdx)
            | _ -> 
                if idx = currentIdx then (Ok e, currentIdx)
                else (state, currentIdx - 1)

        fold folder (seed, length - 1) list |> fst

    let remove idx list =
        let length = length list
        let folder ((state, currentIdx), e) =
            match state with
            | _ when idx = 0 -> (Error "Cannot remove root", 0)
            | _ when idx >= length -> (Error "Invalid index", idx)
            | Ok list when currentIdx = idx -> (Ok list, currentIdx - 1)
            | Ok list -> (Ok (e::list), currentIdx - 1)
            | Error _ -> (state, currentIdx)

        fold folder (Ok [], length - 1) list 
        |> fst
        |> Result.bind fromList

    let copy idx list = list |> get idx |> Result.map (add list)

    let move idx list = list |> copy idx |> Result.bind (remove idx)

    let private execRange f range list =
        list |> toList |> execRange f range |> Result.bind id

    let private fixRange list =
        function
        | None -> Some (1, length list - 1)
        | some -> some

    let removeRange range list =
        let removeIdxs (a, b) _ =
            List.replicate (b - a + 1) a |> foldResult remove list

        range |> fixRange list |> execRange removeIdxs <| list

    let copyRange range list =
        let copyIdxs (a, b) _ = [ a .. b ] |> foldResult copy list
        execRange copyIdxs range list

    let moveRange range list =
        let moveIdxs (a, b) _ =
            List.replicate (b - a + 1) a |> foldResult move list

        range |> fixRange list |> execRange moveIdxs <| list
