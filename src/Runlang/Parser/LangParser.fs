module LangParser

open FParsec
open Pace
open Interval
open Repetition
open ParserCommons
open LangParserPrimitives

let paceTable = getUserState

let pace =
    let timePace = watchtime .>> pstring "/km" |>> TimePerKm
    let termPace =
        paceTable <*>
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
        ]
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

    reptimes 
    .>>. between (pchar '(') (pchar ')') repetitionValue
    .>> ws 
    |>> RepCount

let replist = sepBy (repcount <|> interval) plus |>> RepList

do repetitionRef := replist;

let repetition = ws >>. repetitionValue .>> ws .>> eof |>> Repetition.toList

let parseWorkout = runParser repetition
