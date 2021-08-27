module PaceTableParserTests

open FsUnit.Xunit
open Xunit
open Utils
open Functions
open PaceTableParser
open Pace
open RunningTerm

[<Fact>]
let ``Parses input file correctly`` () =
    let input =
        [ "CL=11:06/km\n";
          "CA=09:51/km\n";
          "CV=08:52/km\n";
          "TR=04:47/km\n";
          "LVS=04:31/km\n";
          "LE=04:14/km\n";
          "MO=04:01/km\n";
          "FO=03:42/km\n";
          "FTS=03:25/km\n";
          "MAX=02:57/km\n" ]
        |> System.String.Concat

    let expected =
        [ "11:06/km";
          "9:51/km";
          "8:52/km";
          "4:47/km";
          "4:31/km";
          "4:14/km";
          "4:01/km";
          "3:42/km";
          "3:25/km";
          "2:57/km" ]

    input
    |> parsePaceTable
    |> ok
    |> flip List.map [ CL; CA; CV; TR; LVS; LE; MO; FO; FTS; MAX ]
    |> List.map Pace.toString
    |> should equal expected
