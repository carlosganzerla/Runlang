module Cli

open System
open LangParser
open PaceTableParser
open FitEncoder
open IntervalTree
open IntervalEncoder
open FitWorkout
open RunningTerm
open Pace
open Functions
open StringUtils
open ConsoleUtils
open Args
open System.IO


type AppInput =
    { EncodingMode: FitEncodingMode;
      NoExport: bool;
      PaceTable: (RunningTerm -> Pace) option;
      WorkoutName: string;
      WorkoutPath: string;
      WorkoutString: string }

[<Literal>]
let PaceTableFileName = ".pacetable"

[<Literal>]
let BadArguments = -1

[<Literal>]
let BadWorkoutString = -2

[<Literal>]
let PaceTableNotFound = -3

[<Literal>]
let BadPaceTable = -3


let homePath = Environment.GetFolderPath Environment.SpecialFolder.Personal

let defaultPaceFlePath () =
    [ PaceTableFileName; appendPath PaceTableFileName homePath ]
    |> List.filter File.Exists
    |> List.tryHead

let workoutPath workoutName =
    let isGarminDrive (drive: DriveInfo) =
        drive.IsReady
        && drive.VolumeLabel |> toUpper |> contains "GARMIN"

    let exportDirectory (drive: DriveInfo) =
        appendPath "GARMIN" >> appendPath "NEWFILES"
        <| drive.RootDirectory.FullName

    DriveInfo.GetDrives ()
    |> Array.filter isGarminDrive
    |> Array.map exportDirectory
    |> Array.filter Directory.Exists
    |> Array.tryHead
    |> Option.defaultValue homePath
    |> appendPath $"{workoutName}.fit"


let evaluateInput args =
    let encodingMode =
        if args.OpenDistance then
            FitEncodingMode.OpenDistance
        else
            FitEncodingMode.Default

    let paceTable =
        if args.DisplayTree then
            args.PaceTablePath
            |> Option.filter File.Exists
            |> Option.orElseWith defaultPaceFlePath
            |> exitOnNone PaceTableNotFound "Could not find pace table file"
            |> File.ReadAllText
            |> parsePaceTable
            |> exitOnError BadPaceTable
            |> Some
        else
            None

    let workoutName =
        Option.defaultValue $"Runlang_{DateTime.Now}" args.WorkoutName

    let workoutPath =
        args.WorkoutPath
        |> Option.map thunk
        |> Option.defaultValue (delay workoutPath workoutName)
        <| ()

    { PaceTable = paceTable;
      NoExport = args.NoExport;
      WorkoutName = workoutName;
      WorkoutPath = workoutPath;
      WorkoutString = args.WorkoutString;
      EncodingMode = encodingMode }

let displayTree tree table =
    tree
    |> WorkoutTree.toIntervalTree table
    |> IntervalTree.toString
    |> printfn "%s"

let run input =
    let tree =
        input.WorkoutString
        |> parseWorkout
        |> exitOnError BadWorkoutString

    Option.iter (displayTree tree) input.PaceTable
    => if not input.NoExport then
           tree
           |> WorkoutTree.encode input.WorkoutName input.EncodingMode
           |> FitWorkout.dumpFile input.WorkoutPath
           => printfn "Saved workout successfully on path %s" input.WorkoutPath

let app argv =
    parseArgs argv
    |> exitOnNone BadArguments ""
    |> evaluateInput
    |> run
