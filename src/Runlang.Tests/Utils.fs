module Utils
open System

let ok = function
    | Ok ok -> ok
    | Error err -> raise (new Exception($"{err}"))

let shouldBeError = function
    | Error _ -> ()
    | _ -> raise (new Exception("Expected error got Ok"))
