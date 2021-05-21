module PaceFileParser

open FParsec
open ParserCommons
open LangParserPrimitives

let termAndPace term =
    ws >>. term .>> pchar '=' .>> ws .>>. timePace
    .>> ws
    .>> newline

let terms =
    sequence [ termAndPace pCL
               termAndPace pCA
               termAndPace pCV
               termAndPace pTR
               termAndPace pLVS
               termAndPace pLE
               termAndPace pMO
               termAndPace pFO
               termAndPace pFTS
               termAndPace pMAX ]
    |>> Map.ofList

let parseFile = runParser terms ()
