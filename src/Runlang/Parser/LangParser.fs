module LangParser

open FParsec
open LangParseTree
open ParserCommons
open LangParserPrimitives

type LangParser = Parser<WorkoutTree list, unit>

let pace = (timePace |>> Absolute) <|> (termPace |>> Term)

// let progression =
//     distance .>> ws1 .>>. pace .>> pchar '~' .>>. pace
//     |>> fun ((dist, first), last) -> Interval.fromProgression dist first last

let distanceAndPace =
    distance .>> ws1 .>>. pace
    |>> DistanceAndPace
    |>> List.singleton

let timeAndPace =
    time .>> ws1 .>>. pace
    |>> TimeAndPace
    |>> List.singleton

let timeAndDistance =
    time .>> ws1 .>>. distance
    |>> TimeAndDistance
    |>> List.singleton

let step =
    tryMany [ distanceAndPace
              timeAndPace
              timeAndDistance ]
    |>> List.map Step
    .>> ws
    

let plus = (pchar '/' <|> pchar '+') .>> ws

let times = pchar 'x' .>> ws

let repeatTree, repeatRef = createParserForwardedToRef ()

let repeat =
    let repeatCount = attempt (puint32 .>> times)

    repeatCount
    .>>. between (pchar '(') (pchar ')') repeatTree
    .>> ws
    |>> Repeat
    |>> List.singleton

let steps = sepBy (repeat <|> step) plus |>> List.collect id

do repeatRef := steps

let workoutTree: LangParser = ws >>. repeatTree .>> ws .>> eof

let parseWorkout = runParser workoutTree ()
