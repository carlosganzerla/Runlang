module PaceFileParser

open FParsec
open ParserUtils
open LangParserPrimitives

let termAndPace term = term .>> pchar '=' .>>. timePace .>> newline

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
    .>> eof
    |>> Map.ofList

let parseTerms = runParser terms ()
