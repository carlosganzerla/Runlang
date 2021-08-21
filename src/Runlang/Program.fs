open Functions

[<EntryPoint>]
let rec main argv =
    match Array.tryHead argv with
    | Some "--interactive"
    | Some "-i" -> argv |> Array.tail |> InteractiveCli.app
    | Some "--encoder"
    | Some "-e" -> EncoderCli.app ()
    | _ ->
        printfn "Error: bad arguments!"
        => System.Environment.Exit -1

    0
