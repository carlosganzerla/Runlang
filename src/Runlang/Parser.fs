module Parser

open FParsec
open Models

type Parser<'t> = Parser<'t, unit>

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
    <?> """Format <Hours>h<Minutes>min<Seconds>s or 0:00:00"""

let pace =
     watchtime .>> pstring "/km" |>> TimePerKm
     <?> "Format 00:00/km"

let distanceAndPace =
    distance .>> ws1 .>>. pace
    |>> (DistanceAndPace >> Interval.create)

let timeAndPace =
    time .>> ws1 .>>. pace
    |>> (TimeAndPace >> Interval.create)

let timeAndDistance =
    time .>> ws1 .>>. distance
    |>> (TimeAndDistance >> Interval.create)

let interval =
    ws >>. tryMany [
        distanceAndPace;
        timeAndPace;
        timeAndDistance;
    ] .>> ws .>> eof

// let plus = pchar '/' <|> pchar '+'

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
