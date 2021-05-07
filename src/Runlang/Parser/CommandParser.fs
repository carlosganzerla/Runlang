module CommandParser

open FParsec
open Manipulation
open RootList

type CommandParser<'t> = Parser<'t, RootList<Manipulation>>
