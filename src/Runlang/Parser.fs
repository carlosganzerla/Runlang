module Parser

open FParsec
open Models
open System
open System.Text.RegularExpressions

type Parser<'t> = Parser<'t, unit>

let inline charToUint c = uint c - uint '0'

let ws = spaces1

let pinteger = puint32

let pwatchDigits =
    regex "[0-5][0-9]" |>> uint
    <?> "Expected 00 to 59"

let pbaseSixty =
     regex "[0-5]?[0-9]" |>> uint
    <?> "Expected a number betwenn 0 and 59"


let zero = preturn 0u

let createTime ((h,min),s) = Time.create h min s

let pdecimal =
    let dot = opt (pchar '.')

    let partsToDecimal intpart decpart =
        decimal $"{intpart}.{decpart}"

    let decimalPart (intpart, dot) =
        match dot with
        | Some _ -> pinteger |>> partsToDecimal intpart
        | None -> preturn intpart |>> decimal

    pinteger .>>. dot >>= decimalPart

let pdistance : Parser<_> =
    let pdistanceM =
        let m = pchar 'm'
        pinteger .>> m |>> Meters .>> ws

    let pdistanceKm =
        let km = pstring "km"
        pdecimal .>> km  |>> Kilometers .>> ws

    attempt pdistanceM <|> pdistanceKm

let pwatchtime: Parser<_> =
    let pcolon = pchar ':'
    let phhmmss =
        pinteger
        .>> pcolon
        .>>. pwatchDigits
        .>> pcolon
        .>>. pwatchDigits
        .>> ws
    let pmmss = zero .>>. pbaseSixty .>> pcolon .>>. pwatchDigits .>> ws
    choice [
        attempt phhmmss
        pmmss
    ] |>> createTime


let pnumerictime: Parser<_> =
    let ph = pinteger .>> pchar 'h'
    let pmin = pbaseSixty .>> pstring "min"
    let ps = pbaseSixty .>> pchar 's'
    let phmins = ph .>>. pmin .>>. ps .>> ws
    let phmin = ph .>>. pmin .>>. zero .>> ws
    let phs = ph .>>. zero .>>. ps .>> ws
    let pmins = zero .>>. pmin .>>. ps .>> ws
    let ph = ph .>>. zero .>>. zero .>> ws
    let pmin = zero .>>. pmin .>>. zero .>> ws
    let ps = zero .>>. zero .>>. ps .>> ws
    choice [
        attempt phmins
        attempt phmin
        attempt phs
        attempt pmins
        attempt ph
        attempt pmin
        ps
    ] |>> createTime


let ppace = pwatchtime .>> pstring "/km"

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
