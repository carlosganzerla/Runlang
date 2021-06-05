module ParserCommons

open FParsec

let (<*>) pf px = pf >>= (|>>) px

let (|*>) px f = preturn f .>>. px |>> fun (f, (x, y)) -> f x y

let ws1 = spaces1

let ws = spaces

let integer = pint32

let zero input = preturn 0 input

let runParser parser state input =
    let output = runParserOnString parser state "" input

    match output with
    | Success (result, _, _) -> Result.Ok result
    | Failure (errorMsg, _, _) -> Result.Error errorMsg

let tryMany parsers = parsers |> List.map attempt |> choice

let result =
    function
    | Result.Ok ok -> preturn ok
    | Result.Error err -> fail err

let pdecimal input =
    let dot = opt (anyOf ",.")

    let partsToDecimal intpart decpart = decimal $"{intpart}.{decpart}"

    let decimalPart (intpart, dot) =
        match dot with
        | Some _ -> integer |>> partsToDecimal intpart
        | None -> preturn intpart |>> decimal

    (integer .>>. dot >>= decimalPart) input

let sequence parsers x =
    let andThen result next = result .>>. next |>> fun (r, n) -> n :: r

    let p = parsers |> List.fold andThen (preturn []) |>> List.rev in
    p x
