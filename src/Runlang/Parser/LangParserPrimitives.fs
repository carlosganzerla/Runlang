module LangParserPrimitives

open FParsec
open ParserCommons
open Distance
open Time

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
