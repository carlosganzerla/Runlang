module LangParser

open FParsec
open Pace
open Distance
open Time
open Interval
open Repetition
open ParserCommons

type LangParser<'t> = Parser<'t, PaceTable>

let paceTable : LangParser<_> = getUserState

let watchDigits =
    regex "[0-5][0-9]" |>> uint

let baseSixty =
     regex "[0-5]?[0-9]" |>> uint

let createTime ((h,min),s) = Time.create h min s

let distance : LangParser<_> =
    let distanceM =
        let m = pchar 'm'
        uinteger .>> m |>> Meters

    let distanceKm =
        let km = pstring "km"
        pdecimal .>> km  |>> Kilometers

    tryMany [ distanceM; distanceKm ]

let watchtime: LangParser<_> =
    let pcolon = pchar ':'
    let phhmmss = uinteger .>> pcolon .>>. watchDigits .>> pcolon .>>. watchDigits
    let pmmss = uzero .>>. baseSixty .>> pcolon .>>. watchDigits
    tryMany [
        phhmmss
        pmmss
    ] |>> createTime >>= result

let numerictime: LangParser<_> =
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
    tryMany [
        phmins
        phmin
        phs
        pmins
        ph
        pmin
        ps
    ] |>> createTime >>= result

let time =
    tryMany [ numerictime; watchtime ]

let pace =
    let timePace = watchtime .>> pstring "/km" |>> TimePerKm
    let termPace =
        tryMany [
            stringReturn "CL" CL
            stringReturn "CA" CA
            stringReturn "CV" CV
            stringReturn "TR" TR
            stringReturn "LVS" LVS
            stringReturn "LE" LE
            stringReturn "MO" MO
            stringReturn "FO" FO
            stringReturn "FTS" FTS
            stringReturn "MAX" MAX
        ] .>>. paceTable |>> fun (term, table) -> table term
    timePace <|> termPace

let progression =
    distance .>> ws1 .>>. pace .>> pchar '~' .>>. pace
    |>> fun ((dist, first), last) -> Interval.fromProgression dist first last

let distanceAndPace =
    distance .>> ws1 .>>. pace
    |>> (DistanceAndPace >> Interval.create)
    |>> List.singleton

let timeAndPace =
    time .>> ws1 .>>. pace
    |>> (TimeAndPace >> Interval.create)
    |>> List.singleton

let timeAndDistance =
    time .>> ws1 .>>. distance
    |>> (TimeAndDistance >> Interval.create)
    |>> List.singleton

let interval =
    tryMany [
        progression;
        distanceAndPace;
        timeAndPace;
        timeAndDistance;
    ]
    |>> List.map Interval
    |>> RepList
    .>> ws

let plus = (pchar '/' <|> pchar '+') .>> ws

let times = pchar 'x' .>> ws

let repetitionValue, repetitionRef = createParserForwardedToRef ()

let repcount =
    let reptimes = attempt (uinteger .>> times)
    reptimes .>>. between (pchar '(') (pchar ')') repetitionValue
    .>> ws |>> RepCount

let replist = sepBy (repcount <|> interval) plus |>> RepList

do repetitionRef := replist;

let repetition = ws >>. repetitionValue .>> ws .>> eof |>> Repetition.toList

let parseWorkout = runParser repetition
