module LangParser

open FParsec
open WorkoutTree
open WorkoutStep
open ProgressionStep
open WorkoutPace
open ParserUtils
open LangParserPrimitives

type LangParser = Parser<WorkoutTree, unit>

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
    |>> WorkoutTree.step
    .>> ws

let plus = (pchar '/' <|> pchar '+') .>> ws

let times = pchar 'x' .>> ws

let repeatTree, repeatRef = createParserForwardedToRef ()

let repeat =
    let repeatCount = attempt (puint32 .>> times)

    preturn WorkoutTree.repeat
    <*> repeatCount
    <*> between (pchar '(') (pchar ')') repeatTree
    .>> ws
    >>= result

let steps = sepBy (repeat <|> step) plus
do repeatRef := steps

let workoutTree : LangParser =
    ws >>. repeatTree .>> ws .>> eof |>> WorkoutTree.repeat 1u
    >>= result

let parseWorkout = runParser workoutTree ()
