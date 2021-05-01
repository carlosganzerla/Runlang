module Utils

let fromResultList list = 
    let folder list next = 
        Result.bind (fun list ->
            Result.bind (fun x -> Ok (x::list)) next
        ) list
    List.fold folder (Ok []) list

