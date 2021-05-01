module RootList

open Utils

type RootList<'T> =
    | Root of 'T
    | Cons of 'T * RootList<'T>

[<RequireQualifiedAccess>]
module RootList =
    let create = Root

    let add list manipulation = Cons (manipulation, list)

    let rec cata fCons fRoot = function
        | Cons (e, list) ->
            let result = cata fCons fRoot list
            fCons (e, result)
        | Root r -> fRoot r

    let private k x _ = x

    let length list = cata (snd >> (+) 1) (k 1) list

    let root list = cata (snd >> id) id list

    let top list = cata (fst >> id) id list

    let toList list = 
        cata (fun (e, es) -> e::es) List.singleton list |> List.rev

    let get idx list =
        let length = length list

        let getOnIdx (e, (previous, count)) =
            if idx = count then (e, count + 1)
            else (previous, count + 1)

        let fRoot r = (r, 1)

        let getFn = cata getOnIdx fRoot >> fst

        if idx >= length || idx < 0 then
            Error "Invalid index"
        else
            Ok (getFn list)

    let remove idx list =
        let length = length list

        let removeOnIdx (e, (list, count)) =
            if idx = count then (list, count + 1)
            else (Cons (e, list), count + 1)

        let fRoot r = (Root r, 1) 

        let removeFn = cata removeOnIdx fRoot >> fst

        if idx >= length || idx < 0 then
            Error "Invalid index"
        elif idx = 0 then
            Error "Cannot remove root"
        else
            Ok (removeFn list) 

    let copy idx list =
        list |> get idx |> Result.map (add list)

    let move idx list =
        list |> copy idx |> Result.bind (remove idx) 
