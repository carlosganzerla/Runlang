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

let rangeScope = 
    pint32 .>> pchar '-' .>>. pint32 
    |>> fun (x, y) -> (x - 1, y - 1)
    |>> Some

let listScope start = pchar '.' >>% fun m -> (start, List.length m - 1)

let indexScope: CommandParser<_> = 
    pint32 |>> (+) -1 |>> fun x -> konst (x, x)

let scope firstIdx =
    tryMany [
        listScope firstIdx;
        rangeScope;
        indexScope;
    ]

let rootSwitch = pstring "-r" >>% (RootList.root >> Result.Ok)

let indexSwitch = pstring "-m" .>> ws1 >>. pint32 |>> RootList.get

let noSwitch = preturn (RootList.top >> Result.Ok) 

let switch =
    (rootSwitch 
    <|> indexSwitch
    <|> noSwitch)
    <*> currentList
    >>= result


let rangeCommand pcommand firstIdx = 
    pcommand .>> ws1 .>>. scope firstIdx

// let manipulationCommand pcommand =
//     let applyFunction =
//         rangeCommand pcommand 0 
//         .>> ws
//         |*> switch 
//         |>> fun (range, m) -> (range m, m)
//         |*> f
//         >>= result
//         
//     currentList .>>. applyFunction |*> RootList.add |>> Updated

let join: CommandParser<_> = 
    let join = pstring "join" >>% Manipulation.join
    join

let split: CommandParser<_> =
    let splitTime = time |>> TimeSplit
    let splitDistance = distance |>> DistanceSplit
    let splitValue = tryMany [ splitTime; splitDistance; ]
    let split = pstring "split" .>> ws1 >>% Manipulation.split <*> splitValue
    split
    

let pnew: CommandParser<_> = stringReturn "new" New  

let commands = pnew

let command = ws >>. commands .>> ws .>> eof

let parseCommand = runParser command 
