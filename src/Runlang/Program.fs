open Functions

let badArguments () = printfn "Error: bad arguments!" => exit -1

let tail = Array.tail

[<EntryPoint>]
let rec main argv =
    argv
    |> Array.tryHead
    |> function
        | Some "--interactive"
        | Some "-i" -> InteractiveCli.app <| tail argv
        | Some "--encoder"
        | Some "-e" -> EncoderCli.app <| tail argv
        | _ -> badArguments ()

    0
