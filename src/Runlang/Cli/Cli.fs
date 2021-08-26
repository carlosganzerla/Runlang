module EncoderCli

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
      DisplayTree: bool;
      PaceTable: RunningTerm -> Pace;
      WorkoutName: string;
      WorkoutPath: string;
      WorkoutString: string }

[<Literal>]
let PaceTableFileName = ".pacetable"

let homePath = Environment.GetFolderPath Environment.SpecialFolder.Personal

let paceTablePath () =
    [ PaceTableFileName; appendPath PaceTableFileName homePath ]
    |> List.filter File.Exists
    |> List.tryHead
    |> Option.defaultValue (exit<string> -2)


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

    let pacetable =
        args.PaceTablePath
        |> Option.map thunk
        |> Option.defaultValue (paceTablePath)
        <| ()
        |> parsparseP

    let workoutName =
        Option.defaultValue
            $"Runlang_{DateTime.Now:dd_MM_yyyy_hh_mm_ss}"
            args.WorkoutName

    let workoutPath =
        args.WorkoutPath
        |> Option.map thunk
        |> Option.defaultValue (delay workoutPath workoutName)
        <| ()

    { DisplayTree = args.DisplayTree;
      PaceTablePath = paceTablePath;
      WorkoutName = workoutName;
      WorkoutPath = workoutPath;
      WorkoutString = args.WorkoutString;
      EncodingMode = encodingMode }

let displayTree input tree =
    if input.DisplayTree then
        tree
        |> WorkoutTree.toIntervalTree
        |> IntervalTree.toString
        |> printf "%s" 
        => tree
    else
        tree

let downloadWorkout input =
    let workout =
        input.WorkoutString
        |> parseWorkout
        |> function
            | Ok tree -> tree
            | Error error ->
                printf "%s" error => exit -1

        |> displayTree input

    let steps = WorkoutTree.toFit mode tree
    let name = readMandatory "Enter workout name"

    let defaultPath =
        findGarmin ()
        |> Option.defaultValue homePath
        |> appendPath $"{name}.fit"

    let encoding =
        FitWorkout.createWorkout name
        |> List.fold (flip FitWorkout.addStep)
        <| steps

    readOptional "Enter file path" defaultPath
    |> FitWorkout.dumpFile encoding
    => printfn $"Success!"

let app argv =
    parseArgs argv
    |> Result.map evaluateInput
    |> Result.map downloadWorkout
    |> Result.mapError (printf "%s" >> delay exit<unit> -1)
