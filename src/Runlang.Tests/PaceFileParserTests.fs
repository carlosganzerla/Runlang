module PaceFileParserTests

open FsUnit.Xunit
open Xunit
open Utils
open PaceFileParser
open Pace

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
        [ (CL, "11:06/km");
          (CA, "9:51/km");
          (CV, "8:52/km");
          (TR, "4:47/km");
          (LVS, "4:31/km");
          (LE, "4:14/km");
          (MO, "4:01/km");
          (FO, "3:42/km");
          (FTS, "3:25/km");
          (MAX, "2:57/km") ]

    input
    |> parseTerms
    |> ok
    |> Map.toList
    |> List.map (fun (t, p) -> (t, Pace.toString p))
    |> should equal expected
