module ParserCommons

open FParsec
open Distance
open Time

let (<*>) pf px = pf >>= (|>>) px

let (|*>) px f = preturn f .>>. px |>> fun (f, (x, y)) -> f x y

let ws1 = spaces1

let ws = spaces

let uinteger = puint32

let uzero input = preturn 0u input

let runParser parser state input =
    let output = runParserOnString parser state "" input
    match output with
    | Success (result, _, _)  -> Result.Ok result
    | Failure (errorMsg, _, _) -> Result.Error errorMsg

let tryMany parsers =
    parsers |> List.map attempt |> choice

let result = function
    | Result.Ok ok -> preturn ok
    | Result.Error err -> fail err

let pdecimal input =
    let dot = opt (pchar '.')

    let partsToDecimal intpart decpart =
        decimal $"{intpart}.{decpart}"

    let decimalPart (intpart, dot) =
        match dot with
        | Some _ -> uinteger |>> partsToDecimal intpart
        | None -> preturn intpart |>> decimal

    (uinteger .>>. dot >>= decimalPart) input

