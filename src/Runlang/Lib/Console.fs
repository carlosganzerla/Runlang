module ConsoleUtils

open System
open StringUtils

let read = Console.ReadLine >> trim

let clear = Console.Clear

let rec readMandatory desc =
    printfn "%s: " desc
    |> read
    |> function
        | "" -> readMandatory desc
        | value -> value

let readOptional desc fallback =
    printfn "%s (%s): " desc fallback
    |> read
    |> function
        | "" -> fallback
        | value -> value
