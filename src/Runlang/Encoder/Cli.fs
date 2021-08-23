module EncoderCli

open System
open LangParser
open EncoderExtensions
open EncodedWorkout
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

let downloadWorkout tree =
    let steps = WorkoutTree.encode (StepEncodingMode.Default) tree
    let name = readMandatory "Enter workout name"

    let defaultPath =
        findGarmin ()
        |> Option.defaultValue homePath
        |> appendPath $"{name}.fit"

    let encoding =
        EncodedWorkout.createEncoding name
        |> List.fold (flip EncodedWorkout.addStep)
        <| steps

    readOptional "Enter file path" defaultPath
    |> EncodedWorkout.dumpFile encoding
    => printfn $"Success!"


let rec app () =
    readMandatory "Enter workout string"
    |> parseWorkout
    |> function
        | Ok tree -> downloadWorkout tree
        | Error error -> printfn "%s" error
    |> app
