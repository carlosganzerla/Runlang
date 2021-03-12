module Parser

open FParsec
open Models
open System
open System.Text.RegularExpressions

type Parser<'t> = Parser<'t, unit>

let inline charToUint c = uint c - uint '0'

let ws = spaces1

let pinteger = puint32

let zero = preturn 0u

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

let pwatchtime: Parser<_> =
    let phhmmss = regex "([0-9]+):([0-5][0-9]):([0-5][0-9])"
    let pmmss = regex "([0-5]?[0-9]):([0-5][0-9])"
    let ptimestr = phhmmss <|> pmmss
    let toTime (str: string) =
        let time =
            str.Split [| ':' |]
            |> Array.toList
            |> List.map uint
        match time with
        | [h; m; s;] -> preturn (Time.create h m s)
        | [m; s;] -> preturn (Time.create 0u m s)
        | _ -> fail $"Bad time string: {str}"
    ptimestr >>= toTime

let pnumerictime: Parser<_> =
    let ph = pinteger .>> pchar 'h'
    let pmin = pinteger .>> pstring "min"
    let ps = pinteger .>> pchar 's'
    let phmins = ph .>>. pmin .>>. ps
    let phmin = ph .>>. pmin .>>. zero
    let phs = ph .>>. zero .>>. ps
    let pmins = zero .>>. pmin .>>. ps
    let ph = ph .>>. zero .>>. zero
    let pmin = zero .>>. pmin .>>. zero
    let ps = zero .>>. zero .>>. ps
    choice [
        attempt phmins
        attempt phmin
        attempt phs
        attempt pmins
        attempt ph
        attempt pmin
        ps
    ]

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
