module CommandParser

open FParsec
open Manipulation
open RootList
open ParserCommons

type AppState =
    | New
    | Updated of ManipulationList

type CommandParser<'t> = Parser<'t, RootList<Manipulation>>

let konst x = fun _ -> x

let currentList = getUserState

let list = pstring "list" >>. currentList |>> Updated

let rangeScope: CommandParser<_> = 
    pint32 .>> pchar '-' .>>. pint32 
    |>> fun (x, y) -> (x - 1, y - 1)
    |>> konst

let listScope start: CommandParser<_> = 
    pchar '.' 
    |>> fun _ -> fun m -> (start, List.length m - 1)

let indexScope: CommandParser<_> = 
    pint32 
    |>> (+) -1
    |>> fun x -> konst (x, x)

let scope firstIdx =
    tryMany [
        listScope firstIdx;
        rangeScope;
        indexScope;
    ]

let rootSwitch = pstring "-r" >>. preturn (RootList.root >> Result.Ok)

let indexSwitch = 
    pstring "-m" .>> ws1 >>. pint32 |>> RootList.get

let noSwitch = preturn (RootList.top >> Result.Ok) 

let switch = 
    (rootSwitch <|> indexSwitch <|> noSwitch)
    .>>. currentList 
    |>> (fun (f, r) -> (f r)) >>= result

let join = 
    pstring "join" >>.
    ws1 >>.
    scope 0 .>>
    ws .>>.
    switch 
    |>> fun (f, m) -> (f m, m)
    |>> fun (s, m) -> Manipulation.join s m
    >>= result
    .>>. currentList
    |>> fun (m, l) -> RootList.add l m
    |>> Updated

let pnew = pstring "new" >>. preturn New

let commands = join <|> pnew

let command = ws >>. commands .>> ws .>> eof

let parseCommand = runParser command 
