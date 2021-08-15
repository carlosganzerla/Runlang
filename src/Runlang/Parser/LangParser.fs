module LangParser

open FParsec
open WorkoutTree
open WorkoutStep
open ProgressionStep
open WorkoutPace
open ParserCommons
open LangParserPrimitives

type LangParser = Parser<WorkoutTree list, unit>

let pace = (timePace |>> Absolute) <|> (termPace |>> Term)

let progression =
    preturn ProgressionStep.create <*> distance .>> ws1 <*> pace
    .>> pstring "->"
    <*> pace
    .>> pchar ':'
    <*> distance
    |>> Progression

let distanceAndPace = distance .>> ws1 .>>. pace |>> DistanceAndPace

let timeAndPace = time .>> ws1 .>>. pace |>> TimeAndPace

let timeAndDistance = time .>> ws1 .>>. distance |>> TimeAndDistance

let step =
    tryMany [ progression
              distanceAndPace
              timeAndPace
              timeAndDistance ]
    |>> Step
    .>> ws

let plus = (pchar '/' <|> pchar '+') .>> ws

let times = pchar 'x' .>> ws

let repeatTree, repeatRef = createParserForwardedToRef ()

let repeat =
    let repeatCount = attempt (puint32 .>> times)

    repeatCount .>>. between (pchar '(') (pchar ')') repeatTree
    .>> ws
    |>> Repeat

let steps = sepBy (repeat <|> step) plus

do repeatRef := steps

let workoutTree: LangParser = ws >>. repeatTree .>> ws .>> eof

let parseWorkout = runParser workoutTree ()
