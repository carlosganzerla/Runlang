module LangParserPrimitives

open FParsec
open ParserCommons
open Distance
open Time
open Pace

let watchDigits x = let p = regex "[0-5][0-9]" |>> uint in p x

let baseSixty x = let p = regex "[0-5]?[0-9]" |>> uint in p x

let createTime ((h, min), s) = Time.create h min s

let distance x =
    let distanceM =
        let m = pchar 'm'
        uinteger .>> m |>> Meters

    let distanceKm =
        let km = pstring "km"
        pdecimal .>> km |>> Kilometers

    let p = tryMany [ distanceM; distanceKm ] in
    p x

let watchtime x =
    let colon = pchar ':'

    let phhmmss =
        uinteger .>> colon .>>. watchDigits .>> colon
        .>>. watchDigits

    let pmmss = uzero .>>. baseSixty .>> colon .>>. watchDigits

    let p = tryMany [ phhmmss; pmmss ] |>> createTime >>= result in
    p x

let numerictime x =
    let ph = uinteger .>> pchar 'h'
    let pmin = baseSixty .>> pstring "min"
    let ps = baseSixty .>> pchar 's'
    let phmins = ph .>>. pmin .>>. ps
    let phmin = ph .>>. pmin .>>. uzero
    let phs = ph .>>. uzero .>>. ps
    let pmins = uzero .>>. pmin .>>. ps
    let ph = ph .>>. uzero .>>. uzero
    let pmin = uzero .>>. pmin .>>. uzero
    let ps = uzero .>>. uzero .>>. ps

    let p =
        tryMany [ phmins
                  phmin
                  phs
                  pmins
                  ph
                  pmin
                  ps ]
        |>> createTime
        >>= result in

    p x

let time x = let p = tryMany [ numerictime; watchtime ] in p x

let timePace x = let p = watchtime .>> pstring "/km" |>> TimePerKm in p x

let pCL x = let p = stringReturn "CL" CL in p x
let pCA x = let p = stringReturn "CA" CA in p x
let pCV x = let p = stringReturn "CV" CV in p x
let pTR x = let p = stringReturn "TR" TR in p x
let pLVS x = let p = stringReturn "LVS" LVS in p x
let pLE x = let p = stringReturn "LE" LE in p x
let pMO x = let p = stringReturn "MO" MO in p x
let pFO x = let p = stringReturn "FO" FO in p x
let pFTS x = let p = stringReturn "FTS" FTS in p x
let pMAX x = let p = stringReturn "MAX" MAX in p x

let sequence parsers x =
    let andThen result next = result .>>. next |>> fun (r, n) -> n :: r

    let p = parsers |> List.fold andThen (preturn []) |>> List.rev in
    p x
