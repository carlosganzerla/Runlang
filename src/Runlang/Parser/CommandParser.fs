module CommandParser

open FParsec
open Manipulation
open RootList
open Interval
open ParserCommons
open LangParserPrimitives

type AppState =
    | New
    | Updated of ManipulationList

type CommandParser<'t> = Parser<'t, RootList<Manipulation>>

let currentList = getUserState

let list = pstring "list" >>. currentList |>> Updated

let rangeScope offset =
    pint32 .>> pchar '-' .>>. pint32
    |>> fun (x, y) -> (x - offset, y - offset)
    |>> Some

let listScope = pchar '.' >>% None

let indexScope offset = pint32 |>> (+) -offset |>> fun x -> Some (x, x)

let scope offset =
    tryMany [ listScope
              rangeScope offset
              indexScope offset ]
    .>> ws

let rootSwitch = pstring "-r" >>% (RootList.root >> Result.Ok)

let indexSwitch = pstring "-m" .>> ws1 >>. pint32 |>> RootList.get

let noSwitch = preturn (RootList.top >> Result.Ok)

let switch =
    (rootSwitch <|> indexSwitch <|> noSwitch) <*> currentList
    >>= result

let manipulationCommand pcommand =
    let execCommand = pcommand <*> scope 1 <*> switch >>= result
    currentList .>>. execCommand |*> RootList.add |>> Updated

let join =
    let join = pstring "join" >>% Manipulation.join .>> ws1
    manipulationCommand join

let split : CommandParser<_> =
    let splitTime = time |>> TimeSplit
    let splitDistance = distance |>> DistanceSplit
    let splitValue = tryMany [ splitTime; splitDistance ]

    let split =
        pstring "split" .>> ws1 >>% Manipulation.split
        <*> splitValue
        .>> ws1

    manipulationCommand split

let pnew = stringReturn "new" New

let commands = join <|> split <|> pnew

let command = ws >>. commands .>> ws .>> eof

let parseCommand = runParser command
