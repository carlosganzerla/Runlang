module ArgsParserTests

open FsUnit.Xunit
open Xunit
open Args

[<Fact>]
let ``Parsers only workout`` () =
    parseArgs [ "1km TR" ]
    |> should
        equal
        (Some
            { OpenDistance = false;
              DisplayTree = false;
              WorkoutName = None;
              WorkoutPath = None;
              PaceTablePath = None;
              NoExport = false;
              WorkoutString = "1km TR" })

[<Fact>]
let ``Doest not work without workout`` () =
    parseArgs [ "-d"
                "-o"
                "-p"
                "Path"
                "-n"
                "Name"
                "--table"
                "Table"
                "--no-file" ]
    |> should equal None

[<Fact>]
let ``AllOptions`` () =
    parseArgs [ "-d"
                "-o"
                "-p"
                "Path"
                "-n"
                "Name"
                "--table"
                "Table"
                "--no-file"
                "2km LE" ]
    |> should
        equal
        (Some
            { OpenDistance = true;
              DisplayTree = true;
              WorkoutName = Some "Name";
              WorkoutPath = Some "Path";
              PaceTablePath = Some "Table";
              NoExport = true;
              WorkoutString = "2km LE" })
