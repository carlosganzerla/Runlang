module Utils

open System
open Utils
open Pace
open Interval
open LangParser

let shouldBeError =
    function
    | Error _ -> ()
    | _ -> raise (new Exception ("Expected error got Ok"))
