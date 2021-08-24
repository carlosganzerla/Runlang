module EncoderCli

open System
open LangParser
open FitEncoder
open FitWorkout
open Functions
open StringUtils
open ConsoleUtils
open System.IO

let homePath = Environment.GetFolderPath Environment.SpecialFolder.Personal

let findGarmin () =
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

let downloadWorkout mode tree =
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

let parseArgs args =
    match Array.tryHead args with
    | Some "--open"
    | Some "-o" -> FitEncodingMode.OpenDistance
    | _ -> FitEncodingMode.Default

let rec app args =
    let mode = parseArgs args

    readMandatory "Enter workout string"
    |> parseWorkout
    |> function
        | Ok tree -> downloadWorkout mode tree
        | Error error -> printfn "%s" error
    => app args
