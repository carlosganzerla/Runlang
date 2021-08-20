open Pace
open InteractiveCli
open EncoderCli

[<EntryPoint>]
let rec main argv =
    EncoderCli.app ()
    0
