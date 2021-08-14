open Pace
open InteractiveCli

[<EntryPoint>]
let rec main argv =
    app argv |> ignore
    0
