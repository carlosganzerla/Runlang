// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Parser

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let rec main argv =
    Console.WriteLine "Repetition:"
    |> Console.ReadLine
    |> test repetition
    main argv |> ignore
    0 // return an integer exit code
