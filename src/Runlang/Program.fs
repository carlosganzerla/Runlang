open Pace
open Cli

[<EntryPoint>]
let rec main argv =
    app argv |> ignore
    0
