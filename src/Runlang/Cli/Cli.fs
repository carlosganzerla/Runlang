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
      PaceTable: (RunningTerm -> Pace) option;
      WorkoutName: string;
      WorkoutPath: string;
      WorkoutString: string }

[<Literal>]
let PaceTableFileName = ".pacetable"

let homePath = Environment.GetFolderPath Environment.SpecialFolder.Personal

let defaultPaceFlePath () =
    [  PaceTableFileName; appendPath PaceTableFileName homePath ]
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
            |> exitOnNone -3 "Could not find pace table file"
            |> File.ReadAllText
            |> parsePaceTable
            |> exitOnError -3
            |> Some
        else
            None

    let workoutName =
        Option.defaultValue
            $"Runlang_{DateTime.Now:dd_MM_yyyy_hh_mm_ss}"
            args.WorkoutName

    let workoutPath =
        args.WorkoutPath
        |> Option.map thunk
        |> Option.defaultValue (delay workoutPath workoutName)
        <| ()

    { PaceTable = paceTable;
      WorkoutName = workoutName;
      WorkoutPath = workoutPath;
      WorkoutString = args.WorkoutString;
      EncodingMode = encodingMode }

let displayTree input tree =
    match input.PaceTable with
    | Some table ->
        tree
        |> WorkoutTree.toIntervalTree table
        |> IntervalTree.toString
        |> printfn "%s"
    | None -> ()
    => tree

let run input =
    input.WorkoutString
    |> parseWorkout
    |> exitOnError -3
    |> displayTree input
    |> WorkoutTree.encode input.WorkoutName input.EncodingMode
    |> FitWorkout.dumpFile input.WorkoutPath
    => printfn "Saved workout successfully on path %s" input.WorkoutPath

let app argv = parseArgs argv |> exitOnError -1 |> evaluateInput |> run
