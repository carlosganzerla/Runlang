module LangParserPrimitives

open FParsec
open ParserUtils
open Distance
open Time
open Pace
open RunningTerm

let watchDigits x = let p = regex "[0-5][0-9]" |>> int in p x

let baseSixty x = let p = regex "[0-5]?[0-9]" |>> int in p x

let createTime ((h, min), s) = Time.create h min s

let distance x =
    let distanceM =
        let m = pchar 'm'
        integer .>> m |>> Distance.meters

    let distanceKm =
        let km = pstring "km"
        pdecimal .>> km |>> Distance.kilometers

    let p = tryMany [ distanceM; distanceKm ] in
    p x

let watchtime x =
    let colon = pchar ':'

    let phhmmss =
        integer .>> colon .>>. watchDigits .>> colon
        .>>. watchDigits

    let pmmss = zero .>>. baseSixty .>> colon .>>. watchDigits

    let p = tryMany [ phhmmss; pmmss ] |>> createTime >>= result in
    p x

let numerictime x =
    let ph = integer .>> pchar 'h'
    let pmin = baseSixty .>> pstring "min"
    let ps = baseSixty .>> pchar 's'
    let phmins = ph .>>. pmin .>>. ps
    let phmin = ph .>>. pmin .>>. zero
    let phs = ph .>>. zero .>>. ps
    let pmins = zero .>>. pmin .>>. ps
    let ph = ph .>>. zero .>>. zero
    let pmin = zero .>>. pmin .>>. zero
    let ps = zero .>>. zero .>>. ps

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

let termPace x =
    let p =
        tryMany [ pCL
                  pCA
                  pCV
                  pTR
                  pLVS
                  pLE
                  pMO
                  pFO
                  pFTS
                  pMAX ] in p x
