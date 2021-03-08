module Parser

open FParsec
open Models

type Parser<'t> = Parser<'t, unit>

let inline charToUint c = uint c - uint '0'

let pinteger = puint32

let pbasesixty : Parser<_> =
    let firstDigit = anyOf ['0' .. '5']
    let toUint (d, u) = 10u*(charToUint d) + (charToUint u)
    firstDigit .>>. digit |>> toUint

let pdecimal =
    let dot = opt (pchar '.')
    let partsToDecimal intpart decpart =
        decimal $"{intpart}.{decpart}"
    let decimalPart (intpart, dot) =
        match dot with
        | Some _ -> pinteger |>> partsToDecimal intpart
        | None -> preturn intpart |>> decimal
    pinteger .>>. dot >>= decimalPart

let pdistanceM =
    let m = pchar 'm'
    pinteger .>> m |>> Meters

let pdistanceKm =
    let km = pstring "km"
    pdecimal .>> km  |>> Kilometers

let pdistance : Parser<_> =
    attempt pdistanceM <|> pdistanceKm

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
