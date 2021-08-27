module ConsoleUtils

open System
open StringUtils
open Functions

let exitOnError code =
    function
    | Ok ok -> ok
    | Error error -> printfn "%s" error => exit code

let exitOnNone code error =
    function
    | Some value -> value
    | None -> printfn "%s" error => exit code

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
