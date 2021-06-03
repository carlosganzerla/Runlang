module LangParser

open FParsec
open Interval
open Repetition
open ParserCommons
open LangParserPrimitives

let paceTable = getUserState

let pace =

    let termPace =
        paceTable
        <*> tryMany [ pCL
                      pCA
                      pCV
                      pTR
                      pLVS
                      pLE
                      pMO
                      pFO
                      pFTS
                      pMAX ]

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
    tryMany [ progression
              distanceAndPace
              timeAndPace
              timeAndDistance ]
    |>> List.map Interval
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
    |>> List.singleton

let replist = sepBy (repcount <|> interval) plus |>> List.collect id
do repetitionRef := replist

let repetition = ws >>. repetitionValue .>> ws .>> eof

let parseWorkout = runParser repetition
