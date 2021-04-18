module RootList

type RootList<'T> =
    | Root of 'T
    | Cons of 'T * RootList<'T>

module RootList =
    let create = Root

    let add manipulation list = Cons (manipulation, list)

    let rec cata fCons fRoot = function
        | Cons (e, list) ->
            let result = cata fCons fRoot list
            fCons (e, result)
        | Root r -> fRoot r

    let private k x _ = x

    let length list = cata (snd >> (+) 1) (k 1) list

    let rec root list = cata (snd >> id) id list

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

