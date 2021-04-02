module Parser

open FParsec
open Pace
open Distance
open Time
open Interval
open Repetition

type Parser<'t> = Parser<'t, PaceTable>

let paceTable : Parser<_> = getUserState

let ws1 = spaces1

let ws = spaces

let integer = puint32

let result = function
    | Result.Ok ok -> preturn ok
    | Result.Error err -> fail err

let watchDigits =
    regex "[0-5][0-9]" |>> uint

let baseSixty =
     regex "[0-5]?[0-9]" |>> uint

let tryMany parsers =
    parsers |> List.map attempt |> choice

let zero = preturn 0u

let createTime ((h,min),s) = Time.create h min s

let pdecimal =
    let dot = opt (pchar '.')

    let partsToDecimal intpart decpart =
        decimal $"{intpart}.{decpart}"

    let decimalPart (intpart, dot) =
        match dot with
        | Some _ -> integer |>> partsToDecimal intpart
        | None -> preturn intpart |>> decimal

    integer .>>. dot >>= decimalPart

let distance : Parser<_> =
    let distanceM =
        let m = pchar 'm'
        integer .>> m |>> Meters

    let distanceKm =
        let km = pstring "km"
        pdecimal .>> km  |>> Kilometers

    tryMany [ distanceM; distanceKm ]

let watchtime: Parser<_> =
    let pcolon = pchar ':'
    let phhmmss = integer .>> pcolon .>>. watchDigits .>> pcolon .>>. watchDigits
    let pmmss = zero .>>. baseSixty .>> pcolon .>>. watchDigits
    tryMany [
        phhmmss
        pmmss
    ] |>> createTime >>= result

let numerictime: Parser<_> =
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
    let reptimes = attempt (integer .>> times)
    reptimes .>>. between (pchar '(') (pchar ')') repetitionValue
    .>> ws |>> RepCount

let replist = sepBy (repcount <|> interval) plus |>> RepList

do repetitionRef := replist;

let repetition = ws >>. repetitionValue .>> ws .>> eof |>> Repetition.toList

let parseWorkout paceTable input =
    let output = runParserOnString repetition paceTable "" input
    match output with
    | Success(intervals, _, _)  -> Result.Ok intervals
    | Failure(errorMsg, _, _) -> Result.Error errorMsg

